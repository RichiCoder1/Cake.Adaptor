using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Cake.Common;
using Cake.Core.Annotations;
using Cake.Core.Scripting;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp; 
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Cake.Adaptor.Generator
{
    public class ScriptHostGenerator
    {
        public static void Main(string[] args)
        {
            var references = GetReferences();
            var parseOptions = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, usings: GetUsings().Select(syntax => syntax.Name.ToString()));
            var mainClass = CreateMainClass();
            var compilation = CSharpCompilation.Create("Cake.Adaptor", new List<SyntaxTree> { mainClass }, options: parseOptions);
            compilation = compilation.WithReferences(references);
            var diagnostics = compilation.GetDiagnostics();
            if (diagnostics.Any(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error))
            {
                throw new InvalidOperationException();
            }

            var binPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            compilation.Emit(Path.Combine(binPath, "Cake.Adaptor.dll"));
        }

        private static SyntaxTree CreateMainClass(string cakeScriptPath = null)
        {
            var root = (CompilationUnitSyntax) SyntaxFactory.CompilationUnit(
                externs: new SyntaxList<ExternAliasDirectiveSyntax>(),
                usings: GetUsings(),
                attributeLists: new SyntaxList<AttributeListSyntax>(),
                members: new SyntaxList<MemberDeclarationSyntax>()).SyntaxTree.GetRoot();

            var members = GetScriptHostCoreMembers().AddRange(GetAliasesMembers(typeof(ReleaseNotes).Assembly));

            if (!string.IsNullOrWhiteSpace(cakeScriptPath))
            {
                // Get Cake Script References
                var references = GetCakeScriptAssemblies(cakeScriptPath);

                // Process their members
                members = references.Aggregate(
                    members,
                    (current, reference) => current.AddRange(GetAliasesMembers(reference)));
            }

            root =
                root.WithMembers(
                    ListOf<MemberDeclarationSyntax>(
                        GetNamespace()
                            .WithMembers(
                                ListOf<MemberDeclarationSyntax>(
                                    BuildInitialScriptHost()
                                        .WithMembers(members)))));

            return CSharpSyntaxTree.Create(root, new CSharpParseOptions(LanguageVersion.CSharp5));
        }

        #region Construction

        private static SyntaxList<T> ListOf<T>(T item) where T : SyntaxNode
        {
            return SyntaxFactory.List<T>().Add(item);
        }

        public static NamespaceDeclarationSyntax GetNamespace()
        {
            return SyntaxFactory.NamespaceDeclaration(GetNameSyntax("Cake.Adaptor"));
        }

        public static ClassDeclarationSyntax BuildInitialScriptHost()
        {
            return
                SyntaxFactory.ClassDeclaration(SyntaxFactory.Identifier("ScriptHost"))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword)));
        }

        public static SyntaxList<MemberDeclarationSyntax> GetScriptHostCoreMembers()
        {
            var scriptHostMembers = typeof(IScriptHost).GetMembers();
            var syntaxList = SyntaxFactory.List<MemberDeclarationSyntax>();

            foreach (var member in scriptHostMembers)
            {
                switch (member.MemberType)
                {
                    case MemberTypes.Method:
                        if (member is MethodInfo)
                        {
                            if(member.Name.StartsWith("get_") || member.Name.StartsWith("set_"))
                                continue;
                            syntaxList = syntaxList.Add(GetForMethod(member));
                        }
                        break;
                    case MemberTypes.Property:
                        syntaxList = syntaxList.Add(GetForProperty((PropertyInfo)member));
                        break;
                    default:
                        break;
                }
            }

            return syntaxList;
        }

        public static SyntaxList<MemberDeclarationSyntax> GetAliasesMembers(Assembly assembly)
        {
            var aliases = assembly.GetExportedTypes()
                .Select(type => type.GetMembers(BindingFlags.Public | BindingFlags.Static))
                .SelectMany(lists => lists)
                .Distinct()
                .Where(info => info.GetCustomAttribute<CakeMethodAliasAttribute>() != null)
                .ToList();

            return SyntaxFactory.List(aliases.Select(GetForMethod));
        } 

        public static MemberDeclarationSyntax GetForMethod(MemberInfo member)
        {
            var method = (MethodInfo)member;
            return SyntaxFactory.MethodDeclaration(
                attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                modifiers:
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)),
                returnType: GetTypeName(method.ReturnType),
                explicitInterfaceSpecifier: null,
                typeParameterList: method.IsGenericMethod ? GetTypeParametersForMethod(method) : null,
                identifier: SyntaxFactory.Identifier(method.Name),
                parameterList: GetParametersForMethod(method),
                constraintClauses: SyntaxFactory.List<TypeParameterConstraintClauseSyntax>(),
                body: GetNotImplementedBlockSyntax(),
                semicolonToken: SyntaxFactory.Token(SyntaxKind.None));
        }

        public static TypeParameterListSyntax GetTypeParametersForMethod(MethodInfo method)
        {
            if (!method.IsGenericMethod) return SyntaxFactory.TypeParameterList();

            return
                SyntaxFactory.TypeParameterList(
                    SyntaxFactory.SeparatedList(
                        method.GetGenericArguments()
                            .Select(
                                genericArgument => SyntaxFactory.TypeParameter(genericArgument.Name))));
        }

        public static ParameterListSyntax GetParametersForMethod(MethodInfo method)
        {
            var isAlias = method.GetCustomAttribute<CakeMethodAliasAttribute>() != null;

            var parameterSyntaxList =
                method.GetParameters()
                    .Select(
                        (param, i) =>
                            {
                                if (i == 0 && isAlias)
                                {
                                    return null;
                                }

                                var paramSyntax =
                                    SyntaxFactory.Parameter(SyntaxFactory.Identifier(param.Name))
                                        .WithType(GetTypeName(param.ParameterType));

                                // out Type param
                                if (param.IsOut)
                                {
                                    paramSyntax =
                                        paramSyntax.WithModifiers(
                                            SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.OutKeyword)));
                                }
                                // ref Type param
                                else if (param.ParameterType.IsByRef)
                                {
                                    paramSyntax =
                                        paramSyntax.WithModifiers(
                                            SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.RefKeyword)));
                                }

                                if (param.HasDefaultValue)
                                {
                                    var defaultValue = param.DefaultValue;
                                    if (defaultValue == null)
                                    {
                                        paramSyntax =
                                            paramSyntax.WithDefault(
                                                SyntaxFactory.EqualsValueClause(
                                                    SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)));
                                    }
                                    else
                                    {
                                        LiteralExpressionSyntax literalSyntax;
                                        if (param.DefaultValue is bool)
                                        {
                                            literalSyntax = (bool)param.DefaultValue
                                                                ? SyntaxFactory.LiteralExpression(
                                                                    SyntaxKind.TrueLiteralExpression)
                                                                : SyntaxFactory.LiteralExpression(
                                                                    SyntaxKind.FalseLiteralExpression);
                                        }
                                        else
                                        {
                                            var literal =
                                                typeof(SyntaxFactory).GetMethods(
                                                    BindingFlags.Public | BindingFlags.Static)
                                                    .Where(
                                                        info =>
                                                        info.Name == "Literal" && info.GetParameters().Count() == 1
                                                        && info.GetParameters()[0].ParameterType
                                                        == param.DefaultValue.GetType())
                                                    .ToList()
                                                    .First();

                                            literalSyntax = (LiteralExpressionSyntax)literal.Invoke(null, new[] { param.DefaultValue });
                                        }

                                        paramSyntax =
                                            paramSyntax.WithDefault(SyntaxFactory.EqualsValueClause(literalSyntax));
                                    }
                                }

                                return paramSyntax;
                            });
            return SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(parameterSyntaxList.Where(param => param != null)));
        }

        public static BlockSyntax GetNotImplementedBlockSyntax()
        {
            return SyntaxFactory.Block(
                SyntaxFactory.ThrowStatement(
                    SyntaxFactory.ObjectCreationExpression(
                        SyntaxFactory.Token(SyntaxKind.NewKeyword),
                        SyntaxFactory.ParseTypeName(typeof(NotImplementedException).Name),
                        SyntaxFactory.ArgumentList(),
                        null)));
        }

        public static PropertyDeclarationSyntax GetForProperty(PropertyInfo info)
        {
            return SyntaxFactory.PropertyDeclaration(
                attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                modifiers:
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)),
                type: GetTypeName(info.PropertyType),
                explicitInterfaceSpecifier: null,
                identifier: SyntaxFactory.Identifier(info.Name),
                accessorList: GetAccessor(info),
                expressionBody: null,
                initializer: null);
        }

        public static AccessorListSyntax GetAccessor(PropertyInfo info)
        {
            return
                SyntaxFactory.AccessorList(
                    ListOf(
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.GetAccessorDeclaration,
                            SyntaxFactory.Block(
                                SyntaxFactory.ReturnStatement(
                                    SyntaxFactory.DefaultExpression(GetTypeName(info.PropertyType)))))));
        }

        private static TypeSyntax GetTypeName(Type type)
        {
            if (type.IsByRef)
            {
                return GetTypeName(type.GetElementType());
            }

            if (!type.IsGenericType)
            {
                if (type.IsArray)
                {
                    return SyntaxFactory.ArrayType(GetTypeName(type.GetElementType()));
                }

                if (type == typeof(void)) return SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword));
                if (string.IsNullOrWhiteSpace(type.FullName))
                {
                    return GetNameSyntax(type.Name);
                }
                return GetNameSyntax(type.FullName);
            }
            else
            {
                var nameString = type.FullName.Substring(0, type.FullName.IndexOf('`'));
                var typeName = GetNameSyntax(nameString);
                if (typeName.Kind() == SyntaxKind.IdentifierName)
                {
                    var name = SyntaxFactory.GenericName(nameString);
                    name =
                        name.WithTypeArgumentList(
                            SyntaxFactory.TypeArgumentList(
                                SyntaxFactory.SeparatedList(type.GetGenericArguments().Select(GetTypeName))));
                    return name;
                }
                else
                {
                    var lastPart = ((QualifiedNameSyntax)typeName).Right;
                    var genericName = SyntaxFactory.GenericName(lastPart.Identifier.Text);
                    genericName =
                        genericName.WithTypeArgumentList(
                            SyntaxFactory.TypeArgumentList(
                                SyntaxFactory.SeparatedList(type.GetGenericArguments().Select(GetTypeName))));
                    return ((QualifiedNameSyntax)typeName).WithRight(genericName);
                }
            }
        }

        private static NameSyntax GetNameSyntax(string name)
        {
            var splitName = name.Split('.');
            if (splitName.Length == 1) return SyntaxFactory.IdentifierName(name);

            return GetQualifiedNameSyntax(splitName);
        }

        private static QualifiedNameSyntax GetQualifiedNameSyntax(System.Collections.Generic.IReadOnlyList<string> nameParts)
        {
            if (nameParts.Count == 2)
            {
                return SyntaxFactory.QualifiedName(
                    SyntaxFactory.IdentifierName(nameParts.First()),
                    SyntaxFactory.IdentifierName(nameParts.Last()));
            }

            return SyntaxFactory.QualifiedName(
                GetQualifiedNameSyntax(nameParts.Take(nameParts.Count - 1).ToList()),
                SyntaxFactory.IdentifierName(nameParts.Last()));
        }

        #endregion

        #region General
        private static SyntaxList<UsingDirectiveSyntax> GetUsings()
        {
            return
                SyntaxFactory.List<UsingDirectiveSyntax>()
                    .AddRange(
                        new[]
                            {
                                SyntaxFactory.UsingDirective(GetNameSyntax("System")),
                                SyntaxFactory.UsingDirective(GetNameSyntax("System.Collections.Generic")),
                                SyntaxFactory.UsingDirective(GetNameSyntax("Cake.Core"))
                            });
        }

        private static MetadataReference[] GetReferences()
        {
            return new []
            {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Uri).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Core.ActionTask).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Common.ReleaseNotes).Assembly.Location)
            };
        }

        private static IEnumerable<Assembly> GetCakeScriptAssemblies(string cakeScriptFile)
        {
            var references = new List<string>();

            using (var stream = File.OpenRead(cakeScriptFile))
            using (var streamReader = new StreamReader(stream))
            {
                while (!streamReader.EndOfStream)
                {
                    var line = streamReader.ReadLine();
                    if (line.Trim().StartsWith("#r"))
                    {
                        var start = line.IndexOf('"');
                        var end = line.LastIndexOf('"') - start;
                        references.Add(line.Substring(start, end));
                    }
                }
            }

            return references.Select(Assembly.LoadFile);
        }

#endregion


 
    }

}
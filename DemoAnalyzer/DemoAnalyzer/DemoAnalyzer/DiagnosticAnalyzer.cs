using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace DemoAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class DemoAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "DemoAnalyzer";
        public const string RethrowDiagnosticId = "RethrowDemoAnalyzer";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Naming";

        private static readonly LocalizableString RethrowTitle = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString RethrowMessageFormat = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString RethrowDescription = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string RethrowCategory = "Usage";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        private static DiagnosticDescriptor RethrowRule = new DiagnosticDescriptor(RethrowDiagnosticId, RethrowTitle, RethrowMessageFormat, RethrowCategory, DiagnosticSeverity.Error, isEnabledByDefault: true, description: RethrowDescription);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule, RethrowRule); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
            context.RegisterSyntaxNodeAction<SyntaxKind>(AnalyzeNode, SyntaxKind.ThrowStatement);
        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            // Find just those named type symbols with names containing lowercase letters.
            if (namedTypeSymbol.Name.ToCharArray().Any(char.IsLower))
            {
                // For all such symbols, produce a diagnostic.
                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

                context.ReportDiagnostic(diagnostic);
            }
        }

        private void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var throwStatement = (ThrowStatementSyntax)context.Node;
            ExpressionSyntax expr = throwStatement.Expression;
            if (expr == null)
            {
                return;
            }

            for (SyntaxNode syntax = throwStatement; syntax != null; syntax = syntax.Parent)
            {
                switch (syntax.Kind())
                {
                    case SyntaxKind.CatchClause:
                        {
                            var local = context.SemanticModel.GetSymbolInfo(expr).Symbol as ILocalSymbol;
                            if (local == null || local.Locations.Length == 0)
                            {
                                return;
                            }

                            // if (local.LocalKind != LocalKind.Catch) return; // TODO: expose LocalKind in the symbol model?

                            var catchClause = syntax as CatchClauseSyntax;
                            if (catchClause != null && catchClause.Declaration.Span.Contains(local.Locations[0].SourceSpan))
                            {
                                var diagnostic = Diagnostic.Create(RethrowRule, throwStatement.GetLocation());
                                context.ReportDiagnostic(diagnostic);
                                return;
                            }
                        }

                        break;

                    case SyntaxKind.ParenthesizedLambdaExpression:
                    case SyntaxKind.SimpleLambdaExpression:
                    case SyntaxKind.AnonymousMethodExpression:
                    case SyntaxKind.ClassDeclaration:
                    case SyntaxKind.StructDeclaration:
                        return;
                }
            }
        }
    }
}

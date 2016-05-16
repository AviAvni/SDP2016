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
    public class RethrowAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "RethrowAnalyzer";
        internal static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        internal static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.RethrowAnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        internal const string Category = "Usage";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction<SyntaxKind>(AnalyzeThrowNode, SyntaxKind.ThrowStatement);
        }

        private void AnalyzeThrowNode(SyntaxNodeAnalysisContext context)
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

                            var catchClause = syntax as CatchClauseSyntax;
                            if (catchClause != null)
                            {
                                var declaredSymbol = context.SemanticModel.GetDeclaredSymbol(catchClause.Declaration);
                                if (local == declaredSymbol)
                                {
                                    var diagnostic = Diagnostic.Create(Rule, throwStatement.GetLocation());
                                    context.ReportDiagnostic(diagnostic);
                                    return;
                                }
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
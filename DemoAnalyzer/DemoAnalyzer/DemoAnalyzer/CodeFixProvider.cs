using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace DemoAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(DemoAnalyzerCodeFixProvider)), Shared]
    public class DemoAnalyzerCodeFixProvider : CodeFixProvider
    {
        private const string title = "Make uppercase";
        private const string rethrowTitle = "Make rethrow";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(DemoAnalyzerAnalyzer.DiagnosticId, DemoAnalyzerAnalyzer.RethrowDiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest
            var diagnostic = context.Diagnostics.First();
            if (diagnostic.Id == DemoAnalyzerAnalyzer.DiagnosticId)
            {
                var diagnosticSpan = diagnostic.Location.SourceSpan;

                // Find the type declaration identified by the diagnostic.
                var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

                // Register a code action that will invoke the fix.
                context.RegisterCodeFix(
                    CodeAction.Create(
                        title: title,
                        createChangedSolution: c => MakeUppercaseAsync(context.Document, declaration, c),
                        equivalenceKey: title),
                    diagnostic);
            }
            else
            {
                var diagnosticSpan = diagnostic.Location.SourceSpan;

                // Find the throw statement by the diagnostic.
                var throwStatement = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ThrowStatementSyntax>().First();

                // Register a code action that will invoke the fix.
                context.RegisterCodeFix(
                    CodeAction.Create(
                        title: rethrowTitle,
                        createChangedSolution: c => MakeRethrowAsync(context.Document, throwStatement, c),
                        equivalenceKey: rethrowTitle),
                    diagnostic);
            }
        }

        private async Task<Solution> MakeUppercaseAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            // Compute new uppercase name.
            var identifierToken = typeDecl.Identifier;
            var newName = identifierToken.Text.ToUpperInvariant();

            // Get the symbol representing the type to be renamed.
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);

            // Produce a new solution that has all references to that type renamed, including the declaration.
            var originalSolution = document.Project.Solution;
            var optionSet = originalSolution.Workspace.Options;
            var newSolution = await Renamer.RenameSymbolAsync(document.Project.Solution, typeSymbol, newName, optionSet, cancellationToken).ConfigureAwait(false);

            // Return the new solution with the now-uppercase type name.
            return newSolution;
        }

        private async Task<Solution> MakeRethrowAsync(Document document, ThrowStatementSyntax throwStatement, CancellationToken cancellationToken)
        {
            // Create the rethrow statement.
            var rethrowExpression = SyntaxFactory.ThrowStatement();

            // Get document root node.
            var root = await document.GetSyntaxRootAsync();

            // Replace the throw statement with rethrow statement.
            var newRoot = root.ReplaceNode(throwStatement, rethrowExpression);

            // Create new document with new root
            var newDocument = document.WithSyntaxRoot(newRoot);

            // Return the new solution with the rethrow statement.
            return newDocument.Project.Solution;
        }
    }
}
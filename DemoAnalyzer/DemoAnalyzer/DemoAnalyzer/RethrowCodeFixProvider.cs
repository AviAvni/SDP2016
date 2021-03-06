﻿using System;
using System.Composition;
using System.Collections.Generic;
using System.Collections.Immutable;
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
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(RethrowCodeFixProvider)), Shared]
    public class RethrowCodeFixProvider : CodeFixProvider
    {
        private const string title = "Make rethrow";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(RethrowAnalyzer.DiagnosticId); }
        }

        public override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public async override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();

            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var throwStatement = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ThrowStatementSyntax>().First();

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: title,
                    createChangedSolution: c => MakeRethrowAsync(context.Document, throwStatement, c),
                    equivalenceKey: title),
                diagnostic);
        }

        private async Task<Solution> MakeRethrowAsync(Document document, ThrowStatementSyntax throwStatement, CancellationToken cancellationToken)
        {
            var rethrowStatement = SyntaxFactory.ThrowStatement();

            var root = await document.GetSyntaxRootAsync();

            var newRoot = root.ReplaceNode(throwStatement, rethrowStatement);

            var newDocument = document.WithSyntaxRoot(newRoot);

            return newDocument.Project.Solution;
        }
    }
}
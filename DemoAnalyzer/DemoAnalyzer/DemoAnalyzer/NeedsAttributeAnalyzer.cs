﻿using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Semantics;

namespace DemoAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class NeedsAttributeAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "NeedsAttributeAnalyzer";
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.GenericAnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.GenericAnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.GenericAnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction<SyntaxKind>(AnalyzeInvocationNode, SyntaxKind.InvocationExpression);
            //context.RegisterOperationAction(AnalyzeInvocationOperation, OperationKind.InvocationExpression);
        }

        private void AnalyzeInvocationNode(SyntaxNodeAnalysisContext context)
        {
            var invocationExpression = (InvocationExpressionSyntax)context.Node;
            if (invocationExpression.ArgumentList.Arguments.Count == 0 ||
                context.SemanticModel.GetDiagnostics(invocationExpression.Span).Any(d => d.Severity == DiagnosticSeverity.Error))
                return;

            var symbolInfo = context.SemanticModel.GetSymbolInfo(invocationExpression, context.CancellationToken);
            var targetMethod = symbolInfo.Symbol as IMethodSymbol;
            if (targetMethod == null)
                return;

            for (int i = 0; i < targetMethod.Parameters.Length; i++)
            {
                var parameter = targetMethod.Parameters[i];
                var attribute = parameter.GetAttributes()
                    .FirstOrDefault(attr => attr.AttributeClass.Name == "NeedsAttribute");
                if (attribute == null || attribute.ConstructorArguments.Length != 1 || 
                    attribute.ConstructorArguments[0].Kind != TypedConstantKind.Type)
                    continue;

                var attributeType = (INamedTypeSymbol)attribute.ConstructorArguments[0].Value;

                var argument = invocationExpression.ArgumentList.Arguments[i];
                var argumentExpression = argument.Expression;
                var argumentType = (INamedTypeSymbol)context.SemanticModel
                    .GetTypeInfo(argumentExpression, context.CancellationToken).Type;

                string typeError;
                if (!ValidateArgumentType(attributeType, argumentType, out typeError))
                {
                    var diagnostic = Diagnostic.Create(Rule, argument.GetLocation(), typeError, attributeType.Name);
                    context.ReportDiagnostic(diagnostic);
                }
            }
        }

        private static bool ValidateArgumentType(INamedTypeSymbol attributeType, INamedTypeSymbol argumentType, out string typeError)
        {
            typeError = null;

            if (!argumentType.IsGenericType)
            {
                if (!argumentType.GetAttributes().Any(attr => attr.AttributeClass == attributeType))
                {
                    typeError = argumentType.Name;
                    return false;
                }
            }
            else
            {
                var innerArgumentType = argumentType.TypeArguments[0];
                if (!innerArgumentType.GetAttributes().Any(attr => attr.AttributeClass == attributeType))
                {
                    typeError = innerArgumentType.Name;
                    return false;
                }
            }

            return true;
        }

        private void AnalyzeInvocationOperation(OperationAnalysisContext context)
        {
            var invocationExpression = (IInvocationExpression)context.Operation;
            var targetMethod = invocationExpression.TargetMethod;
            if (targetMethod == null)
                return;

            foreach (var parameter in targetMethod.Parameters)
            {
                var attribute = parameter.GetAttributes()
                    .FirstOrDefault(attr => attr.AttributeClass.Name == "NeedsAttribute");
                if (attribute == null || attribute.ConstructorArguments.Length != 1 || 
                    attribute.ConstructorArguments[0].Kind != TypedConstantKind.Type)
                    continue;

                var attributeType = (INamedTypeSymbol)attribute.ConstructorArguments[0].Value;

                var argument = invocationExpression.GetArgumentMatchingParameter(parameter);
                var argumentType = (INamedTypeSymbol)argument.Value.Type;
                if (argument.Value.Kind == OperationKind.ConversionExpression)
                {
                    argumentType = (INamedTypeSymbol)((IConversionExpression)argument.Value)
                        .Operand.Type;
                }
                if (argumentType == null)
                    return;

                string typeError;
                if (!ValidateArgumentType(attributeType, argumentType, out typeError))
                {
                    var diagnostic = Diagnostic.Create(Rule, argument.Syntax.GetLocation(), typeError, attributeType.Name);
                    context.ReportDiagnostic(diagnostic);
                }
            }
        }
    }
}
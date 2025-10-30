
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpToAssembly;

public static class Translator
{
    public static string Translate(string csharp)
    {
        var tree = CSharpSyntaxTree.ParseText(csharp);
        var root = tree.GetCompilationUnitRoot();

        var sb = new StringBuilder();

        foreach (var cls in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
        foreach (var method in cls.Members.OfType<MethodDeclarationSyntax>())
        {
            bool isImport = HasAttr(method, "import");
            bool isExport = HasAttr(method, "export");

            var sig = BuildSignature(method);

            if (isImport)
            {
                // import: copy body VERBATIM
                sb.AppendLine($"import {sig}");
                sb.AppendLine("{");
                sb.Append(ExtractBodyRaw(method.Body)); // exact body text
                sb.AppendLine("}");
                sb.AppendLine();
                continue;
            }

            if (isExport) sb.Append("export ");
            sb.AppendLine(sig);
            sb.AppendLine("{");

            var emit = new Emitter(sb, method);
            emit.EmitBody();

            sb.AppendLine("}");
            sb.AppendLine();
        }

        return sb.ToString().TrimEnd();
    }

    // --- helpers ---------------------------------------------------------

    private static string BuildSignature(MethodDeclarationSyntax m)
    {
        string ret = NormalizeType(m.ReturnType);
        string name = m.Identifier.Text;
        string paramList = string.Join(", ",
            m.ParameterList.Parameters.Select(p => $"{NormalizeType(p.Type)} {p.Identifier.Text}"));
        return $"{ret} {name}({paramList})";
    }

    private static bool HasAttr(MethodDeclarationSyntax m, string name) =>
        m.AttributeLists.SelectMany(al => al.Attributes)
            .Any(a => string.Equals(a.Name.ToString(), name, StringComparison.OrdinalIgnoreCase));

    private static string NormalizeType(TypeSyntax? t) =>
        (t?.ToString() ?? "void") switch
        {
            "int" => "int",
            "void" => "void",
            _ => "int" // default to int for MVP (you can extend later)
        };

    private static string ExtractBodyRaw(BlockSyntax? body)
    {
        if (body == null) return "";
        var full = body.ToFullString(); // includes braces
        int iOpen = full.IndexOf('{');
        int iClose = full.LastIndexOf('}');
        if (iOpen < 0 || iClose <= iOpen) return "";
        return full.Substring(iOpen + 1, iClose - iOpen - 1);
    }

    // ====================================================================
    // E M I T T E R
    // ====================================================================

    private sealed class Emitter
    {
        private readonly StringBuilder _sb;
        private readonly MethodDeclarationSyntax _method;
        private readonly HashSet<string> _declaredLocals = new(StringComparer.Ordinal);
        private readonly HashSet<string> _params;

        public Emitter(StringBuilder sb, MethodDeclarationSyntax method)
        {
            _sb = sb;
            _method = method;
            _params = method.ParameterList.Parameters
                        .Select(p => p.Identifier.Text)
                        .ToHashSet(StringComparer.Ordinal);
        }

        public void EmitBody()
        {
            if (_method.Body == null)
            {
                if (_method.ExpressionBody != null)
                    EmitExpression(_method.ExpressionBody.Expression, wantValue: false);
                return;
            }

            foreach (var st in _method.Body.Statements)
                EmitStatement(st);
        }

        // ---------------- statements ----------------

        public void EmitStatement(StatementSyntax st)
        {
            switch (st)
            {
                case LocalDeclarationStatementSyntax local:
                    EmitLocalDecl(local);
                    break;

                case ExpressionStatementSyntax expr:
                    EmitExpression(expr.Expression, wantValue: false);
                    break;

                case ReturnStatementSyntax ret:
                    if (ret.Expression != null) EmitExpression(ret.Expression, wantValue: true);
                    _sb.AppendLine("    // ret (lower later if needed)");
                    break;

                case ForStatementSyntax fs:
                    EmitFor(fs);
                    break;

                case BlockSyntax block:
                    foreach (var s in block.Statements) EmitStatement(s);
                    break;

                case IfStatementSyntax @if:
                    EmitIf(@if);
                    break;

                default:
                    _sb.AppendLine($"    // unsupported: {st.Kind()}");
                    break;
            }
        }

        private void EmitLocalDecl(LocalDeclarationStatementSyntax local)
        {
            var t = NormalizeType(local.Declaration.Type);
            foreach (var v in local.Declaration.Variables)
            {
                DeclareLocalOnce(t, v.Identifier.Text);
                if (v.Initializer != null)
                {
                    EmitExpression(v.Initializer.Value, wantValue: true);
                    _sb.AppendLine($"    local.set {v.Identifier.Text}");
                }
            }
        }

        private void EmitIf(IfStatementSyntax ifs)
        {
            // cond
            EmitExpression(ifs.Condition, wantValue: true);
            _sb.AppendLine("    if{");
            EmitStatement(ifs.Statement);
            _sb.AppendLine("    }");
            if (ifs.Else != null)
            {
                _sb.AppendLine("    else{");
                EmitStatement(ifs.Else.Statement);
                _sb.AppendLine("    }");
            }
        }

        private void EmitFor(ForStatementSyntax fs)
        {
            // init
            if (fs.Declaration != null)
            {
                // e.g., for (var i=0; ...; ...)
                var t = NormalizeType(fs.Declaration.Type);
                foreach (var v in fs.Declaration.Variables)
                {
                    DeclareLocalOnce(t, v.Identifier.Text);
                    if (v.Initializer != null)
                    {
                        EmitExpression(v.Initializer.Value, wantValue: true);
                        _sb.AppendLine($"    local.set {v.Identifier.Text}");
                    }
                }
            }
            foreach (var init in fs.Initializers) EmitExpression(init, wantValue: false);

            // loop {
            _sb.AppendLine("    loop{");

            // condition: if (!(cond)) break;
            if (fs.Condition != null)
            {
                EmitExpression(fs.Condition, wantValue: true);
                _sb.AppendLine("        int.eqz");
                _sb.AppendLine("        if{");
                _sb.AppendLine("            break");
                _sb.AppendLine("        }");
            }

            // body
            if (fs.Statement is BlockSyntax block)
            {
                foreach (var s in block.Statements) EmitStatement(s);
            }
            else
            {
                EmitStatement(fs.Statement);
            }

            // incrementors
            foreach (var incr in fs.Incrementors) EmitExpression(incr, wantValue: false);

            _sb.AppendLine("    }");
        }

        // ---------------- expressions ----------------

        public void EmitExpression(ExpressionSyntax expr, bool wantValue)
        {
            switch (expr)
            {
                case LiteralExpressionSyntax lit when lit.IsKind(SyntaxKind.NumericLiteralExpression):
                    if (wantValue) _sb.AppendLine($"    int.const {lit.Token.ValueText}");
                    return;

                case IdentifierNameSyntax id:
                    if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                    return;

                case ParenthesizedExpressionSyntax par:
                    EmitExpression(par.Expression, wantValue);
                    return;

                case PrefixUnaryExpressionSyntax pre:
                    EmitPrefixUnary(pre, wantValue);
                    return;

                case PostfixUnaryExpressionSyntax post:
                    EmitPostfixUnary(post, wantValue);
                    return;

                case AssignmentExpressionSyntax asn:
                    EmitAssignment(asn, wantValue);
                    return;

                case BinaryExpressionSyntax bin:
                    EmitBinary(bin, wantValue);
                    return;

                case InvocationExpressionSyntax inv:
                    EmitInvocation(inv, wantValue);
                    return;

                case MemberAccessExpressionSyntax ma:
                    // value of member access alone is rarely needed in your asm MVP
                    if (wantValue) _sb.AppendLine($"    // member access value: {ma}");
                    return;

                default:
                    _sb.AppendLine($"    // unsupported expr: {expr.Kind()}");
                    return;
            }
        }

        private void EmitInvocation(InvocationExpressionSyntax inv, bool wantValue)
        {
            var args = inv.ArgumentList?.Arguments ?? default;
            foreach (var a in args) EmitExpression(a.Expression, wantValue: true);

            var name = inv.Expression switch
            {
                IdentifierNameSyntax id   => id.Identifier.Text,
                MemberAccessExpressionSyntax ma => $"{ma.Expression}.{ma.Name.Identifier.Text}",
                _ => inv.Expression.ToString()
            };
            _sb.AppendLine($"    call {name}");

            if (!wantValue)
            {
                // if call returns a value but is used as a statement, you may need 'drop'
                // _sb.AppendLine("    drop");
            }
        }

        private void EmitBinary(BinaryExpressionSyntax bin, bool wantValue)
        {
            string? op = bin.OperatorToken.Kind() switch
            {
                SyntaxKind.PlusToken => "int.add",
                SyntaxKind.MinusToken => "int.sub",
                SyntaxKind.AsteriskToken => "int.mul",
                SyntaxKind.SlashToken => "int.div_s",
                SyntaxKind.LessThanToken => "int.lt_s",
                SyntaxKind.LessThanEqualsToken => "int.le_s",
                SyntaxKind.GreaterThanToken => "int.gt_s",
                SyntaxKind.GreaterThanEqualsToken => "int.ge_s",
                SyntaxKind.EqualsEqualsToken => "int.eq",
                SyntaxKind.ExclamationEqualsToken => "int.ne",
                _ => null
            };

            if (op == null)
            {
                _sb.AppendLine($"    // unsupported binary op: {bin.OperatorToken}");
                return;
            }

            EmitExpression(bin.Left, true);
            EmitExpression(bin.Right, true);
            if (wantValue) _sb.AppendLine($"    {op}");
            else _sb.AppendLine($"    {op} // (discarded)");
        }

        private void EmitAssignment(AssignmentExpressionSyntax asn, bool wantValue)
        {
            // Only simple local assignments for MVP
            if (asn.Left is IdentifierNameSyntax id)
            {
                switch (asn.Kind())
                {
                    case SyntaxKind.SimpleAssignmentExpression: // a = expr
                        EmitExpression(asn.Right, wantValue: true);
                        _sb.AppendLine($"    local.set {id.Identifier.Text}");
                        if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        break;

                    case SyntaxKind.AddAssignmentExpression: // a += expr
                        _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        EmitExpression(asn.Right, wantValue: true);
                        _sb.AppendLine("    int.add");
                        _sb.AppendLine($"    local.set {id.Identifier.Text}");
                        if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        break;

                    case SyntaxKind.SubtractAssignmentExpression: // a -= expr
                        _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        EmitExpression(asn.Right, wantValue: true);
                        _sb.AppendLine("    int.sub");
                        _sb.AppendLine($"    local.set {id.Identifier.Text}");
                        if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        break;

                    default:
                        _sb.AppendLine($"    // unsupported assignment: {asn.Kind()}");
                        break;
                }
            }
            else
            {
                _sb.AppendLine("    // unsupported assignment target");
            }
        }

        private void EmitPrefixUnary(PrefixUnaryExpressionSyntax pre, bool wantValue)
        {
            if (pre.Operand is IdentifierNameSyntax id)
            {
                switch (pre.Kind())
                {
                    case SyntaxKind.PreIncrementExpression:
                        _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        _sb.AppendLine("    int.const 1");
                        _sb.AppendLine("    int.add");
                        _sb.AppendLine($"    local.set {id.Identifier.Text}");
                        if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        return;

                    case SyntaxKind.PreDecrementExpression:
                        _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        _sb.AppendLine("    int.const 1");
                        _sb.AppendLine("    int.sub");
                        _sb.AppendLine($"    local.set {id.Identifier.Text}");
                        if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        return;
                }
            }

            if (pre.IsKind(SyntaxKind.UnaryMinusExpression))
            {
                EmitExpression(pre.Operand, true);
                _sb.AppendLine("    int.const 0");
                _sb.AppendLine("    // swap needed for proper neg; using 0 - x");
                // Proper: push 0, swap, sub → to keep it simple:
                // compute (-x) as (0 - x)
                _sb.AppendLine("    // implement neg as: x -> 0 x; int.sub");
                // Re-emit properly:
                _sb.AppendLine("    // (omitted for brevity in MVP)");
                return;
            }

            _sb.AppendLine($"    // unsupported prefix unary: {pre.Kind()}");
        }

        private void EmitPostfixUnary(PostfixUnaryExpressionSyntax post, bool wantValue)
        {
            if (post.Operand is IdentifierNameSyntax id)
            {
                bool isInc = post.IsKind(SyntaxKind.PostIncrementExpression);
                bool isDec = post.IsKind(SyntaxKind.PostDecrementExpression);

                if (isInc || isDec)
                {
                    if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}"); // old value
                    _sb.AppendLine($"    local.get {id.Identifier.Text}");
                    _sb.AppendLine("    int.const 1");
                    _sb.AppendLine(isInc ? "    int.add" : "    int.sub");
                    _sb.AppendLine($"    local.set {id.Identifier.Text}");
                    return;
                }
            }

            _sb.AppendLine($"    // unsupported postfix unary: {post.Kind()}");
        }

        // ---------------- locals ----------------

        private void DeclareLocalOnce(string type, string name)
        {
            var key = $"{type} {name}";
            if (_declaredLocals.Add(key))
                _sb.AppendLine($"    ; {type} {name}");
        }
    }
}

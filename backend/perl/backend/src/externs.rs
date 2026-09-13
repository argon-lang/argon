use perl_parser_core::{Node, NodeKind, Parser};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExternKind {
    Function,
    Method,
    StaticMethod,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternImplementation {
    pub name: String,
    pub kind: ExternKind,
    pub imports: Vec<String>,
    pub declarations: Vec<String>,
    pub expression: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternSource {
    pub package: String,
    pub implementations: Vec<ExternImplementation>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternParseError(pub String);
impl Display for ExternParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
impl std::error::Error for ExternParseError {}

pub fn parse_externs(source: &str) -> Result<ExternSource, ExternParseError> {
    let mut parser = Parser::new(source);
    let ast = parser
        .parse()
        .map_err(|e| err(format!("malformed Perl: {e}")))?;
    if !parser.errors().is_empty() {
        return Err(err(format!("malformed Perl: {}", parser.errors()[0])));
    }

    let NodeKind::Program { statements } = &ast.kind else {
        return Err(err("Perl parser did not produce a program"));
    };
    let packages = statements
        .iter()
        .filter_map(|node| match &node.kind {
            NodeKind::Package { name, block, .. } => Some((name, block)),
            _ => None,
        })
        .collect::<Vec<_>>();
    if packages.len() != 1 {
        return Err(err("extern source must contain exactly one package"));
    }
    let (package, package_block) = packages[0];
    if package_block.is_some() {
        return Err(err("extern package must use the statement form"));
    }

    let mut imports = Vec::new();
    let mut subs = HashMap::new();
    for statement in statements {
        match &statement.kind {
            NodeKind::Use { .. } => imports.push(statement_source(source, statement)?),
            NodeKind::ExpressionStatement { expression }
                if matches!(
                    &expression.kind,
                    NodeKind::FunctionCall { name, args }
                        if name == "require"
                            && matches!(args.as_slice(), [Node { kind: NodeKind::Identifier { .. }, .. }])
                ) =>
            {
                imports.push(statement_source(source, statement)?);
            }
            NodeKind::Subroutine {
                name: Some(name), ..
            } => {
                if subs.insert(name.clone(), statement).is_some() {
                    return Err(err(format!("duplicate subroutine {name}")));
                }
            }
            _ => {}
        }
    }
    let manifest = subs
        .get("argon_externs")
        .ok_or_else(|| err("missing argon_externs subroutine"))?;
    let categories = hash_entries(manifest_return(manifest)?, source)?;
    let mut implementations = Vec::new();
    let mut seen = HashSet::new();
    for (label, kind) in [
        ("functions", ExternKind::Function),
        ("methods", ExternKind::Method),
        ("static_methods", ExternKind::StaticMethod),
    ] {
        let body = categories
            .iter()
            .find_map(|(name, value)| (name == label).then_some(*value))
            .ok_or_else(|| err(format!("missing mandatory {label} category")))?;
        for (name, expression) in hash_entries(body, source)? {
            if !seen.insert(name.clone()) {
                return Err(err(format!("duplicate extern name {name}")));
            }
            let mut declarations = Vec::new();
            if let Some(target) = named_coderef(expression) {
                let declaration = subs
                    .get(&target)
                    .ok_or_else(|| err(format!("selected subroutine {target} is not declared")))?;
                declarations.push(node_source(source, declaration)?.to_owned());
            }
            implementations.push(ExternImplementation {
                name,
                kind,
                imports: imports.clone(),
                declarations,
                expression: node_source(source, expression)?.trim().to_owned(),
            });
        }
    }
    Ok(ExternSource {
        package: package.clone(),
        implementations,
    })
}

fn err(value: impl Into<String>) -> ExternParseError {
    ExternParseError(value.into())
}
fn manifest_return(manifest: &Node) -> Result<&Node, ExternParseError> {
    let NodeKind::Subroutine { body, .. } = &manifest.kind else {
        return Err(err("argon_externs is not a subroutine"));
    };
    let NodeKind::Block { statements } = &body.kind else {
        return Err(err("argon_externs does not have a block body"));
    };
    let mut returns = statements
        .iter()
        .filter_map(|statement| match &statement.kind {
            NodeKind::Return { value: Some(value) } => Some(value.as_ref()),
            _ => None,
        });
    let value = returns
        .next()
        .ok_or_else(|| err("argon_externs must return a literal hash"))?;
    if returns.next().is_some() {
        return Err(err("argon_externs must contain exactly one return value"));
    }
    Ok(value)
}

fn hash_entries<'a>(
    node: &'a Node,
    source: &str,
) -> Result<Vec<(String, &'a Node)>, ExternParseError> {
    let NodeKind::HashLiteral { pairs } = &node.kind else {
        return Err(err("manifest category must be a literal hash"));
    };
    let mut result = Vec::with_capacity(pairs.len());
    let mut seen = HashSet::new();
    for (key, value) in pairs {
        let key = constant_key(key, source)?;
        if !seen.insert(key.clone()) {
            return Err(err(format!("duplicate key {key}")));
        }
        result.push((key, value));
    }
    Ok(result)
}

fn constant_key(node: &Node, source: &str) -> Result<String, ExternParseError> {
    if !matches!(
        node.kind,
        NodeKind::String {
            interpolated: false,
            ..
        }
    ) {
        return Err(err("dynamic hash key is not allowed"));
    }
    let text = node_source(source, node)?.trim();
    if let Some(inner) = text.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
        if inner.contains('\\') {
            return Err(err("keys must be constant non-interpolated strings"));
        }
        return Ok(inner.to_owned());
    }
    if let Some(inner) = text.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
        if inner.contains(['\\', '$', '@']) {
            return Err(err("keys must be constant non-interpolated strings"));
        }
        return Ok(inner.to_owned());
    }
    Ok(text.to_owned())
}

fn named_coderef(node: &Node) -> Option<String> {
    let NodeKind::Unary { op, operand } = &node.kind else {
        return None;
    };
    if op != "\\" {
        return None;
    }
    match &operand.kind {
        NodeKind::FunctionCall { name, args } if args.is_empty() => Some(name.clone()),
        _ => None,
    }
}

fn statement_source(source: &str, node: &Node) -> Result<String, ExternParseError> {
    let mut result = node_source(source, node)?.trim().to_owned();
    if !result.ends_with(';') {
        result.push(';');
    }
    Ok(result)
}

fn node_source<'a>(source: &'a str, node: &Node) -> Result<&'a str, ExternParseError> {
    source
        .get(node.location.start..node.location.end)
        .ok_or_else(|| err("Perl parser produced an invalid source span"))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn extracts_categories() {
        let x=parse_externs("package P; use Math::BigInt; sub f { return []; } sub argon_externs { return { functions => { x => \\&f }, methods => {}, static_methods => { 's' => sub { [] } } }; } 1;").unwrap();
        assert_eq!(x.implementations.len(), 2);
        assert_eq!(x.implementations[0].imports, ["use Math::BigInt;"]);
        assert_eq!(x.implementations[0].declarations, ["sub f { return []; }"]);
        assert_eq!(x.implementations[0].expression, "\\&f");
        assert_eq!(x.implementations[1].expression, "sub { [] }");
    }
    #[test]
    fn duplicate_fails() {
        assert!(parse_externs("package P; sub argon_externs{return {functions=>{x=>sub{},x=>sub{}},methods=>{},static_methods=>{}};}1;").is_err());
    }

    #[test]
    fn ignores_manifest_like_syntax_outside_ast_nodes() {
        let source = r#"
            package P;
            use strict;
            my $decoy = 'package Wrong; functions => { fake => \\&missing }';
            sub argon_externs {
                return {
                    functions => { real => sub { "}" } },
                    methods => {},
                    static_methods => {},
                };
            }
            1;
        "#;
        let parsed = parse_externs(source).unwrap();
        assert_eq!(parsed.package, "P");
        assert_eq!(parsed.implementations.len(), 1);
        assert_eq!(parsed.implementations[0].name, "real");
    }
}

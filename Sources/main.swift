
// Combats Boilerplate
extension ExpressibleByStringLiteral where StringLiteralType == String {

    public init(unicodeScalarLiteral value: String) {
        self.init(stringLiteral: value)
    }

    public init(extendedGraphemeClusterLiteral value: String) {
        self.init(stringLiteral: value)
    }
}

enum Token: CustomStringConvertible, ExpressibleByStringLiteral {
    case colon
    case comma
    case ident(String)

    init(stringLiteral: String) {
        self = .ident(stringLiteral)
    }

    var description: String {
        switch self {
        case .colon:
            return ":"

        case .comma:
            return ","

        case .ident(let s):
            return s
        }
    }
}

indirect enum AstNode: CustomStringConvertible {
    case invalid(Token)
    case ident(Token)
    case list([AstNode])
    case declCt(names: [AstNode], type: AstNode?, values: [AstNode])
    case declRt(names: [AstNode], type: AstNode?, values: [AstNode])

    func pretty(_ level: Int = 1) -> String {
        let indent = repeatElement(" ", count: level  * 2).joined()

        var name: String
        var children: [String: [AstNode]] = [:]
        switch self {
        case .invalid:
            return "(inv)"

        case .ident(let v):
            return "'\(v)'"

        case .list(let vals):
            name = "list"
            children[""] = vals

        case .declRt(let names, let type, let values):
            name = "declRt"
            children["names"] = names
            children["type"] = type.flatMap({ [$0] })
            children["values"] = values

        case .declCt(let names, let type, let values):
            name = "declCt"
            children["names"] = names
            children["type"] = type.flatMap({ [$0] })
            children["values"] = values
        }

        var str = "("
        str += name
        str += children.reduce("") { str, pair in
            var str = str
            if !pair.key.isEmpty && !pair.value.isEmpty {
                str += " "
                str += pair.key + ": "
                str += pair.value.map({ $0.pretty() }).joined(separator: ", ")
            } else {
                str += pair.value.reduce("", { $0.0 + "\n\(indent)" + $0.1.pretty(level + 1) })
            }

            return str
        }
        str += ")"
        return str
    }

    var description: String {
        return pretty()
    }
}

func append(_ l: AstNode, _ r: AstNode) -> AstNode {
    switch (l, r) {
    case (.list(let ls), .list(let rs)):
        return .list(ls + rs)

    case (.list(let ls), _):
        return .list(ls + [r])

    case (_, .list(let rs)):
        return .list([l] + rs)

    case (_, _):
        return .list([l, r])
    }
}

func explode(_ n: AstNode) -> [AstNode] {
    if case .list(let vals) = n {
        return vals
    }
    return [n]
}

var tokenStack: [Token] = []

func peek() -> Token? {
    return tokenStack.first
}

func next() {
    tokenStack.removeFirst()
}

func lbp(_ token: Token) -> Int {
    switch token {
    case .ident:
        return 0

    case .colon:
        return 1

    case .comma:
        return 2
    }
}

func nud(_ token: Token) -> AstNode {
    switch token {
    case .ident:
        next()
        return AstNode.ident(token)

    default:
        next()
        return AstNode.invalid(token)
    }
}

func led(_ token: Token, _ left: AstNode) -> AstNode {
    switch token {
    case .comma:
        next()
        let bp = lbp(token) - 1
        let rExpr = expr(bp)
        return append(left, rExpr)

    case .colon:
        next()
        let bp = lbp(token)

        switch peek() {
        case .colon?:
            next()
            let rvalue = expr(bp)
            return AstNode.declCt(names: explode(left), type: nil, values: explode(rvalue))

        default: // 'i: i'
            let type = expr(bp)
            return AstNode.declRt(names: explode(left), type: type, values: [])

            // NOTE(vdka): Parse rvalues
        }

    default:
        return AstNode.invalid(token)
    }
}

func expr(_ rbp: Int = 0) -> AstNode {
    guard let token = peek() else { fatalError() }

    var left = nud(token)
    while let token = peek(), rbp < lbp(token) {
        left = led(token, left)
    }
    return left
}

func parse() -> [AstNode] {
    var nodes: [AstNode] = []
    while !tokenStack.isEmpty {
        let node = expr()
        nodes.append(node)
    }
    return nodes
}

tokenStack = ["x", .comma, "y", .comma, "z", .colon, "f32", .comma, "w", .colon, "f64"]

let nodes = parse()
for node in nodes {
    print(node)
}


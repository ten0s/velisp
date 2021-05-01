const VeStack = require('./VeStack.js')

// :: (any, [any]) -> bool
const find = (y, xs) => {
    for (let x of xs) {
        if (y === x) {
            return true
        }
    }
    return false
}

// :: (string) -> string
const escape = (s) => {
    return s
        .replaceAll('\\'    , '\\\\')
        .replaceAll('"'     , '\\"')
        .replaceAll('\r'    , '\\r')
        .replaceAll('\n'    , '\\n')
        .replaceAll('\t'    , '\\t')
        .replaceAll('\u001b', '\\e')
}

// :: (string) -> string
const unescape = (s) => {
    const a = Array.from(s)
    //console.log(a)
    const b = []
    for (let i = 0; i < a.length; i++) {
        if (a[i] === '\\') {
            switch (a[i+1]) {
            case '\\':
                b.push('\\')
                i++
                break
            case '"':
                b.push('"')
                i++
                break
            case 'r':
                b.push('\r')
                i++
                break
            case 'n':
                b.push('\n')
                i++
                break
            case 't':
                b.push('\t')
                i++
                break
            case 'e':
                b.push('\u001b')
                i++
                break
            default:
                b.push(a[i+1])
                i++
                break
            }
        } else {
            b.push(a[i])
        }
    }
    return b.join('')
}

// TODO: FIXME
// Poor man's replaceAll
if (!String.prototype.replaceAll) {
    String.prototype.replaceAll = function replaceAll(from, to) {
        return this.split(from).join(to)
    }
}

// :: (string) -> bool
// Determine if the given string is recoverable by adding
// one or more right parentheses.
// * Left parenthesis should have a corresponding right parenthesis
// * Parentheses inside double quotes should be ignored
// * Given string should be incomplete
const isRecoverableInput = (s) => {
    const stack = new VeStack()
    let inQuotes = false
    for (let i = 0; i < s.length; i++) {
        const c = s.charAt(i)
        switch (c) {
        case '(':
            if (!inQuotes) {
                stack.push(c)
            }
            break
        case ')':
            if (!inQuotes) {
                if (stack.isEmpty() || stack.pop() != '(') {
                    return false
                }
            }
            break
        case '"':
            if (inQuotes) {
                if (i > 0 && s.charAt(i - 1) != '\\') {
                    inQuotes = !inQuotes
                }
            } else {
                inQuotes = true
            }
            break
        default:
            break
        }
    }
    if (stack.isEmpty()) {
        return false
    }
    while (!stack.isEmpty()) {
        if (stack.pop() != '(') {
            return false
        }
    }
    return true
}

module.exports = {
    find,
    escape,
    unescape,
    isRecoverableInput,
}

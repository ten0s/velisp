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
    isRecoverableInput,
}

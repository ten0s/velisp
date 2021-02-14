class VeStack {
    constructor() {
        this.stack = []
    }

    // :: () -> bool
    isEmpty() {
        return this.stack.length === 0
    }

    // :: () -> non_neg_int
    size() {
        return this.stack.length
    }

    // :: (value) -> int
    push(value) {
        this.stack.push(value)
    }

    // :: () -> value | throw Error
    pop() {
        if (this.stack.length > 0) {
            return this.stack.pop()
        }
        throw new Error('Stack is empty')
    }

    // :: () -> value | throw Error
    top() {
        if (this.stack.length > 0) {
            return this.stack[this.stack.length-1]
        }
        throw new Error('Stack is empty')
    }

}

exports.VeStack = VeStack

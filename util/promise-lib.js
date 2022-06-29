//
// From 'JavaScript: The Definitive Guide 7th edition'
//

// :: ([any], (any) => Promise) => Promise
const promiseSequence = (inputs, promiseMaker) => {
    inputs = [...inputs]

    const handleNextInput = (outputs) => {
        if (!inputs.length) {
            // We're done
            return outputs
        } else {
            // Make next Promise
            return promiseMaker(inputs.shift())
                .then(output => outputs.concat(output))
                .then(handleNextInput)
        }
    }

    return Promise.resolve([]).then(handleNextInput)
}

export {
    promiseSequence,
}

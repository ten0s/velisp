const {Int, Real, Fun, ensureType} = require('../VeLispTypes.js');

exports.initContext = function (context) {
    context.setSym('MIN', new Fun('min', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0);
        }
        let min = ensureType('min:', args[0], [Int, Real]);
        let isReal = min instanceof Real;
        for (let i = 1; i < args.length; i++) {
            const num = ensureType('min:', args[i], [Int, Real]);
            isReal = isReal || num instanceof Real;
            if (!num.lessThan(min).isNil()) {
                min = num;
            }
        }
        if (isReal) {
            return new Real(min.value());
        }
        return min;
    }));
    context.setSym('MAX', new Fun('max', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0);
        }
        let max = ensureType('max:', args[0], [Int, Real]);
        let isReal = max instanceof Real;
        for (let i = 1; i < args.length; i++) {
            const num = ensureType('max:', args[i], [Int, Real]);
            isReal = isReal || num instanceof Real;
            if (!max.lessThan(num).isNil()) {
                max = num;
            }
        }
        if (isReal) {
            return new Real(max.value());
        }
        return max;
    }));
}

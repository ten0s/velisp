const VeRegex = require('./VeRegex.js')

class VeGlob {
    constructor(glob) {
        this.glob = glob
        this.re = this.toRegex(glob)
        //console.log(this.re)
        this.Regex = new VeRegex(this.re)
    }

    // :: (string) -> bool
    test(text) {
        return this.Regex.test(text)
    }

    // :: (string) -> string
    toRegex(wc) {
        const re = []
        for (let i = 0; i < wc.length; i++) {
            switch(wc[i]) {
            case '?':
                re.push('.')
                break
            case '*':
                re.push('.*')
                break
            // TODO
            //case '.':
            //    re.push('\.')
            //    break
            default:
                re.push(`[${wc[i].toLowerCase()}${wc[i].toUpperCase()}]`)
                break
            }
        }
        return re.join('')
    }
}

module.exports = VeGlob

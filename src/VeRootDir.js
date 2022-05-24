// Keep the module in the /src directory!

import {dirname} from 'path'
import {fileURLToPath} from 'url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const __rootdir = dirname(__dirname)

export default __rootdir

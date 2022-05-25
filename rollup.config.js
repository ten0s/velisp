import commonjs from '@rollup/plugin-commonjs'
import resolve from '@rollup/plugin-node-resolve'

// npx rollup src/main.js --format cjs --plugin commonjs --plugin node-resolve --external node-gtk --file pkg/src/bundle.js

export default {
    input: 'src/main.js',
    output: {
        file: 'pkg/src/bundle.js',
        format: 'cjs'
    },
    plugins: [
        resolve({
            preferBuiltins: true,
        }),
        commonjs(),
    ],
    external: [
        'node-gtk',
    ]
};

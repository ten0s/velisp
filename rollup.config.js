import commonjs from '@rollup/plugin-commonjs'
import resolve from '@rollup/plugin-node-resolve'

// npx rollup src/main.js --format cjs --plugin commonjs --plugin node-resolve --external node-gtk --dir pkg/src

export default {
    input: 'src/main.js',
    output: {
        dir: 'pkg/src',
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

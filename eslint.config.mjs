import { defineConfig } from "eslint/config"
import globals from "globals"
import path from "node:path"
import { fileURLToPath } from "node:url"
import js from "@eslint/js"
import { FlatCompat } from "@eslint/eslintrc"

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
    baseDirectory: __dirname,
    recommendedConfig: js.configs.recommended,
    allConfig: js.configs.all
})

export default defineConfig([{
    extends: compat.extends("eslint:recommended"),

    languageOptions: {
        globals: {
            ...globals.browser,
            ...globals.node,
        },

        ecmaVersion: 11,
        sourceType: "module",
    },

    rules: {
        indent: ["error", 4],

        "linebreak-style": ["error", "unix"],

        quotes: ["error", "single"],

        semi: ["error", "never"],

        "no-empty": ["error", {allowEmptyCatch: true}],

        "no-useless-escape": ["off"],

        "no-unused-vars": ["error", {argsIgnorePattern: "^_"}],

        "no-irregular-whitespace": ["error", {skipComments: true}],

        "no-prototype-builtins": ["off"],
    },
}])
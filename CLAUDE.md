# Bash commands

- npm run build: Build the project in production mode
- npm run check: Test, lint, format, and compile
- npm test: runs the Elm tests
- npm run playwright: runs the integration tests
- npm run review: lints the Elm code
- npm run clean: removes all previous built artifacts

# Code style

- Use ES modules (import/export) syntax, not CommonJS (require)
- Destructure imports when possible (eg. import { foo } from 'bar')

# Workflow

- Be sure to run `npm run check` when you’re done making a series of code changes
- Prefer running single tests, and not the whole test suite, for performance

# LacqueR

LacqueR is a jamovi module for evidence-grounded, AI-assisted interpretation of statistical analyses. It uses [NaileR](https://github.com/Sebastien-Le/NaileR) to produce structured statistical evidence and generation prompts, then obtains an interpretation from a language model.

LacqueR 0.1.0 is an alpha release. Its currently available analysis is **AI-assisted QDA**, for sensory quantitative descriptive analysis. The current version uses a locally hosted language model through Ollama.

## Evidence first, interpretation second

Statistical evidence constrains the interpretation, but does not reduce interpretation to a restatement of the evidence.

LacqueR keeps three artifacts distinct and inspectable:

- **Evidence**: the statistical information produced through NaileR.
- **Prompt**: the instructions and evidence supplied to the language model.
- **Model response**: the interpretation returned by the language model.

## Development transparency

Parts of the codebase were developed using AI-assisted code generation. Changes are reviewed, tested, and validated by the maintainer before integration.

## License and links

LacqueR is licensed under the GNU General Public License, version 2 or later (`GPL (>= 2)`).

- [Source repository](https://github.com/Sebastien-Le/LacqueR)
- [Issue tracker](https://github.com/Sebastien-Le/LacqueR/issues)

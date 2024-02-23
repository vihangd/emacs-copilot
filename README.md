# Emacs Copilot

Emacs Copilot is an Emacs package that uses the OpenAI API or Google Gemini to generate Emacs Lisp code and content based on given instructions. This package offers a convenient way to create code snippets or analyze text directly within Emacs.

## Installation

1. Make sure the `request` and `request-deferred` packages are installed. Install them using `M-x package-install RET request RET` and `M-x package-install RET request-deferred RET`.

2. Add the `emacs-copilot.el` file to your Emacs load-path.

3. Add the following lines to your Emacs configuration:

```emacs-lisp
(require 'emacs-copilot)

;; if you want to openai key 
(setq emacs-copilot-api-provider 'openai)
(setq emacs-copilot-openai-api-key "openai key")

;; if you want to use gemini
(setq emacs-copilot-api-provider 'google-gemini)
(setq emacs-copilot-google-gemini-api-key "")
```

Replace `"your_openai_api_key"` with your actual OpenAI API key.

## Usage

Emacs Copilot provides the following interactive functions:

- `emacs-copilot`: Generate Emacs Lisp code for a specified task.
- `emacs-copilot-generate`: Generate content based on a given instruction.
- `emacs-copilot-process`: Process the entire buffer or the selected region based on a provided instruction.

### Generating Emacs Lisp code

To generate Emacs Lisp code for a specific task, call the `emacs-copilot` function:

```
M-x emacs-copilot
```

You will be prompted to enter the task description. The generated code will appear in a new buffer, and you will be asked if you want to evaluate it.

### Generating content

To generate content based on a given instruction, call the `emacs-copilot-generate` function:

```
M-x emacs-copilot-generate
```

You will be prompted to enter the instruction. The generated content will appear in a new buffer called `*GPT-Rewritten*`.

### Processing buffer or selected region

To process the entire buffer or the selected region based on a provided instruction, call the `emacs-copilot-process` function:

```
M-x emacs-copilot-process
```

You will be prompted to enter the instruction. The processed content will appear in a new buffer called `*GPT-Rewritten*`.

## Customization

You can customize the `emacs-copilot-api-key` variable to set your OpenAI API key:

```emacs-lisp
(setq emacs-copilot-api-key "your_openai_api_key")
```

Replace `"your_openai_api_key"` with your actual OpenAI API key.

## License

This Emacs package is distributed under the MIT License.

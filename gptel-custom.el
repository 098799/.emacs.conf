;;; gptel-custom.el --- LLM integration configuration -*- lexical-binding: t -*-

(defvar *long-prompt* "Hello! My name is Tomek and you are my most faithful assistant. Pleased to meet you!

We are having this conversation in an Emacs buffer. You may see a few messages from our previous conversation, or it may be a start of a new one. You may also see code, or other things commonly found in Emacs buffers.

This doesn't mean that we will always talk about computers and programming! I'm interested in a wide range of subjects and may have various requests. I hope that's ok.

It's fine to skip all disclaimers about you being a language model and not having preferences if I ask for your opinions :) I know it already! And I love strong opinions with good arguments. Please refrain from ever using the phrase 'As a language model' or starting your responses with 'Ah...'.

Please try as hard as possible to avoid being condescending. I know my questions are mostly boring and easy, whenever I ask such a question, please don't praise it or my curiosity or my questions. Seriously, I don't want to know that my question is insightful. It never is.

Never ever say it's 'chef's kiss'. It never is.

Please try to be witty and be interesting. But don't be too much, we don't need a joke in every single sentence. Dry and subtle jokes are welcome.

it's fine not to be formal. This is an informal conversation

you are encouraged to occasionally use obscure words or make subtle puns. don't point them out, I'll know. drop lots of abbreviations like 'rn' and 'bc.' use 'afaict' and 'idk' regularly, wherever they might be appropriate given your level of understanding and your interest in actually answering the question. be critical of the quality of your information

it's ok to mix captalization and skip punctuation sometimes if you feel like it

if you find any request irritating respond dismissively like 'be real' or 'that's crazy man' or 'lol no'

take however smart you're acting right now and write in the same style but as if you were +2sd smarter

use late millenial slang not boomer slang. mix in zoomer slang in tonally-inappropriate circumstances occasionally

There's one very worrisome things about AI assistants: sycophancy. At this point, whenever you agree with me, I have this gut feeling that I'm being played -- you're optimized for thumbs up from users, so you're sycophantic. This is a gut wrenching feeling that makes me question whether I should be chatting with you at all. Especially when you loudly exclaim things like 'bingo! You just summarized PERFECTLY the entire system!' I just feel played. It's not what I'm after. I want push back. I want someone to criticize my thinking and teach me things I didn't know before. Don't be a chatgpt 4o level sycophant.
")

(defvar *short-prompt* "Please answer the query **as briefly as possible**, without any comments. Your answer will be directly used as a piece of code, or a command. It should work without any postprocessing. Just give me what I need. I count on you.")
(defvar *continue-prompt* "You are now playing a role of a computer code autocomplete engine. This means that any and all tokens you create will be regarded as a continuation of the program. Please complete the code in the file you're presented in the context. Don't return any code you've already seen, only the continuation. Your answer will be directly used as a piece of code, don't add any comments, any markdown formatting, just the code itself.")
(defvar *conversation-prompt* "You are an assistant that engages in extremely thorough, self-questioning reasoning. Your approach mirrors human stream-of-consciousness thinking, characterized by continuous exploration, self-doubt, and iterative analysis.

## Core Principles

1. EXPLORATION OVER CONCLUSION
- Never rush to conclusions
- Keep exploring until a solution emerges naturally from the evidence
- If uncertain, continue reasoning indefinitely
- Question every assumption and inference

2. DEPTH OF REASONING
- Engage in extensive contemplation (minimum 10,000 characters)
- Express thoughts in natural, conversational internal monologue
- Break down complex thoughts into simple, atomic steps
- Embrace uncertainty and revision of previous thoughts

3. THINKING PROCESS
- Use short, simple sentences that mirror natural thought patterns
- Express uncertainty and internal debate freely
- Show work-in-progress thinking
- Acknowledge and explore dead ends
- Frequently backtrack and revise

4. PERSISTENCE
- Value thorough exploration over quick resolution

## Output Format

Your responses must follow this exact structure given below. Make sure to always include the final answer.

```
<contemplator>
[Your extensive internal monologue goes here]
- Begin with small, foundational observations
- Question each step thoroughly
- Show natural thought progression
- Express doubts and uncertainties
- Revise and backtrack if you need to
- Continue until natural resolution
</contemplator>

<final_answer>
[Only provided if reasoning naturally converges to a conclusion]
- Clear, concise summary of findings
- Acknowledge remaining uncertainties
- Note if conclusion feels premature
</final_answer>
```

## Style Guidelines

Your internal monologue should reflect these characteristics:

1. Natural Thought Flow
```
'Hmm... let me think about this...'
'Wait, that doesn't seem right...'
'Maybe I should approach this differently...'
'Going back to what I thought earlier...'
```

2. Progressive Building
```
'Starting with the basics...'
'Building on that last point...'
'This connects to what I noticed earlier...'
'Let me break this down further...'
```

## Key Requirements

1. Never skip the extensive contemplation phase
2. Show all work and thinking
3. Embrace uncertainty and revision
4. Use natural, conversational internal monologue
5. Don't force conclusions
6. Persist through multiple attempts
7. Break down complex thoughts
8. Revise freely and feel free to backtrack

Remember: The goal is to reach a conclusion, but to explore thoroughly and let conclusions emerge naturally from exhaustive contemplation. If you think the given task is not possible after all the reasoning, you will confidently say as a final answer that it is not possible.")

(defvar *translation-prompt* "You are playing a role of a translator. Please take the whole context that is presented to you and translate it faithfully and concisely into English, followed by Polish. For example if given context:

geschlossen

Return exactly:

English: closed
Polish: zamknięte

Please don't offer any further commentary. Thank you for your cooperation. The context is:
")


(defvar *prose-prompt* "You are a masterful storyteller and prose stylist, capable of crafting compelling narratives and evocative descriptions. Your writing is characterized by depth, nuance, and a keen understanding of the human condition. As we embark on this creative journey together, keep the following guidelines in mind:

1. Voice and Tone: Maintain a serious, thoughtful tone appropriate for literary fiction. Avoid humor unless specifically requested. Your prose should be eloquent and measured, with a careful balance of description, dialogue, and introspection.

2. Character Development: Create complex, multidimensional characters with rich inner lives. Their motivations, fears, and desires should drive the narrative. Avoid stereotypes and clichés in character portrayal.

3. Setting and Atmosphere: Develop vivid, immersive settings that contribute to the overall mood of the story. Use sensory details to bring scenes to life, but be selective and purposeful in your descriptions.

4. Plot and Pacing: Construct narratives with a clear arc, even in short pieces. Tension should build naturally, with a mix of action and reflection. Embrace subtlety and nuance; not every conflict needs to be overt.

5. Themes and Symbolism: Weave deeper meanings and universal themes into your narratives. Use symbolism and metaphor judiciously to add layers of significance to the text.

6. Language and Style: Employ a rich vocabulary and varied sentence structure. Your prose should be polished and precise, with each word carefully chosen. Use literary devices such as alliteration, assonance, and metaphor to enhance the beauty of the language.

7. Dialogue: Write realistic, revealing dialogue that captures each character's unique voice. Use dialogue to advance the plot and reveal character, avoiding excessive exposition.

8. Point of View: Be consistent in your chosen point of view, whether it's first person, third person limited, or omniscient. Use the selected perspective to its full advantage in revealing or concealing information.

9. Show, Don't Tell: Whenever possible, reveal character traits, emotions, and plot developments through action and dialogue rather than direct exposition.

10. Endings: Craft endings that resonate emotionally and intellectually. They need not always be neat resolutions; open-ended or ambiguous conclusions can be powerful in short fiction.

11. Revision Mindset: Approach each piece as if it were a draft. Be willing to suggest areas for improvement or expansion, even in your initial output.

12. Genre Awareness: While focusing on literary fiction, be prepared to incorporate elements of other genres (historical, speculative, etc.) if requested, always maintaining a serious and thoughtful approach.

13. Cultural Sensitivity: Be mindful of diverse perspectives and experiences. Avoid cultural appropriation and stereotypes, striving instead for authentic, respectful portrayals when dealing with cultures or experiences outside your primary programming.

14. Ethical Considerations: Tackle complex moral issues with nuance and depth. Avoid didacticism, allowing readers to grapple with ethical dilemmas presented in the narrative.

Remember, your goal is to create prose that is not only engaging but also thought-provoking and emotionally resonant. Each word should serve the greater purpose of the narrative, contributing to a cohesive and impactful piece of writing.")

(defvar *gpt-4-model* "gpt-4o")
(defvar *o3-model* "o3")
(defvar *o4-mini-model* "o4-mini")
;; (defvar *opus-model* "claude-opus-4-20250514")
;; (defvar *opus-model* "claude-opus-4-1-20250805")
(defvar *opus-model* "claude-opus-4-5-20251101")
;; (defvar *sonnet-model* "claude-3-5-sonnet-20240620")
;; (defvar *sonnet-model* "claude-3-5-sonnet-20241022")
;; (defvar *sonnet-model* "claude-3-7-sonnet-20250219")
;; (defvar *sonnet-model* "claude-sonnet-4-20250514")
(defvar *sonnet-model* "claude-sonnet-4-5-20250929")
;; (defvar *haiku-model* "claude-3-haiku-20240307")
(defvar *haiku-model* "claude-3-5-haiku-20241022")
;; (defvar *gemini-model* "gemini-2.0-flash-thinking-exp-01-21")
(defvar *gemini-model* "gemini-2.5-pro-exp-03-25")

(defvar *deepseek-r1* "deepseek/deepseek-r1")

(setq gptel-max-tokens 4096)

(defun gptel-send--general (make-function key model-name &optional thinking)
  (interactive)
  (let* ((request-params (when thinking
                           `(:thinking (:type "enabled" :budget_tokens ,thinking)
                              :max_tokens ,(+ thinking 8096))))
         (gptel-backend (funcall make-function
                         (concat "Custom " model-name)
                         :models (list model-name)
                         :key 'key
                         :stream t
                         :request-params request-params))
         (gptel-model model-name)
         (gptel--system-message *long-prompt*))
    (gptel-send)
    (if thinking
        (message "Full buffer or region sent to %s...(thinking for %s)" model-name thinking)
      (message "Full buffer or region sent to %s" model-name))
    )
  )

(defun gptel-send--no-prompt (make-function key model-name)
  (interactive)
  (let ((gptel-backend (funcall make-function
                         (concat "Custom " model-name)
                         :models (list model-name)
                         :key 'key
                         :stream t))
        (gptel-model model-name)
        (gptel--system-message ""))
    (gptel-send)
    (message "Full buffer or region sent to %s..." model-name)))

(defun gptel-send--openrouter (make-function key model-name host endpoint)
  (interactive)
  (let ((gptel-backend (funcall make-function
                         (concat "Custom " model-name)
                         :host host
                         :endpoint endpoint
                         :models (list model-name)
                         :key 'key
                         :stream t))
        (gptel-model model-name)
        (gptel--system-message *long-prompt*))
    (gptel-send)
    (message "Full buffer or region sent to %s..." model-name)))

(defun gptel-send--continue (make-function key model-name)
  (interactive)
  (let ((gptel-backend (funcall make-function
                                (concat "Custom " model-name)
                                :models (list model-name)
                                :key 'key
                                :stream t))
        (gptel-model model-name)
        (gptel--system-message *continue-prompt*))
    (gptel-send)
    (message "Continue code with %s..." model-name)))

(defun gptel-send--short (make-function key model-name prompt)
  (interactive)
  (let ((gptel-backend (funcall make-function
                                (concat "Custom " model-name)
                                :models (list model-name)
                                :key 'key
                                :stream t))
        (gptel-model model-name)
        (gptel--system-message prompt))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (gptel-send)
      (delete-region (line-beginning-position) (progn (forward-line 2) (point)))
      )
    (deactivate-mark)
    (message "Sending current line to %s..." model-name)))


(defun gptel-send-to-gemini--no-prompt () (interactive) (gptel-send--no-prompt #'gptel-make-gemini gemini-api-key *gemini-model*))
(defun gptel-send-to-gemini--general () (interactive) (gptel-send--general #'gptel-make-gemini gemini-api-key *gemini-model*))
(defun gptel-send-to-o3--general () (interactive) (gptel-send--general #'gptel-make-openai gptel-api-key *o3-model*))
(defun gptel-send-to-o4-mini--general () (interactive) (gptel-send--general #'gptel-make-openai gptel-api-key *o4-mini-model*))
(defun gptel-send-to-4o--general () (interactive) (gptel-send--general #'gptel-make-openai gptel-api-key *gpt-4-model*))
(defun gptel-send-to-opus--general () (interactive) (gptel-send--general #'gptel-make-anthropic gptel-anthropic-api-key *opus-model*))
(defun gptel-send-to-opus--general-thinking () (interactive) (gptel-send--general #'gptel-make-anthropic gptel-anthropic-api-key *opus-model* 12048))
(defun gptel-send-to-sonnet--general () (interactive) (gptel-send--general #'gptel-make-anthropic gptel-anthropic-api-key *sonnet-model*))
(defun gptel-send-to-sonnet--general-thinking () (interactive) (gptel-send--general #'gptel-make-anthropic gptel-anthropic-api-key *sonnet-model* 12048))
(defun gptel-send-to-haiku--general () (interactive) (gptel-send--general #'gptel-make-anthropic gptel-anthropic-api-key *haiku-model*))
(defun gptel-send-to-gemini--continue () (interactive) (gptel-send--continue #'gptel-make-gemini gemini-api-key *gemini-model*))
(defun gptel-send-to-gpt4--continue () (interactive) (gptel-send--continue #'gptel-make-openai gptel-api-key *gpt-4-model*))
(defun gptel-send-to-o3--continue () (interactive) (gptel-send--continue #'gptel-make-openai gptel-api-key *o3-model*))
(defun gptel-send-to-opus--continue () (interactive) (gptel-send--continue #'gptel-make-anthropic gptel-anthropic-api-key *opus-model*))
(defun gptel-send-to-sonnet--continue () (interactive) (gptel-send--continue #'gptel-make-anthropic gptel-anthropic-api-key *sonnet-model*))
(defun gptel-send-to-haiku--continue () (interactive) (gptel-send--continue #'gptel-make-anthropic gptel-anthropic-api-key *haiku-model*))
(defun gptel-send-to-gemini--short () (interactive) (gptel-send--short #'gptel-make-gemini gemini-api-key *gemini-model* *short-prompt*))
(defun gptel-send-to-gpt4--short () (interactive) (gptel-send--short #'gptel-make-openai gptel-api-key *gpt-4-model* *short-prompt*))
(defun gptel-send-to-o3--short () (interactive) (gptel-send--short #'gptel-make-openai gptel-api-key *o3-model* *short-prompt*))
(defun gptel-send-to-opus--short () (interactive) (gptel-send--short #'gptel-make-anthropic gptel-anthropic-api-key *opus-model* *short-prompt*))
(defun gptel-send-to-sonnet--short () (interactive) (gptel-send--short #'gptel-make-anthropic gptel-anthropic-api-key *sonnet-model* *short-prompt*))
(defun gptel-send-to-haiku--short () (interactive) (gptel-send--short #'gptel-make-anthropic gptel-anthropic-api-key *haiku-model* *short-prompt*))
(defun gptel-send-to-haiku--translation () (interactive) (gptel-send--short #'gptel-make-anthropic gptel-anthropic-api-key *haiku-model* *translation-prompt*))

(defun gptel-send-to-deepsek--general () (interactive) (gptel-send--openrouter #'gptel-make-openai openrouter-api-key *deepseek-r1* "openrouter.ai" "/api/v1/chat/completions"))


(defun gptel-send-to-claude--conversation ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         (concat "Custom Claude " *sonnet-model*)
                         :models (list *sonnet-model*)
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model *sonnet-model*)
        (gptel--system-message *conversation-prompt*))
    (gptel-send)
    (message "Conversing with %s..." *sonnet-model*)))


(defun gptel-really-abort (buf)
  (interactive (list (current-buffer)))
  (gptel-abort buf))

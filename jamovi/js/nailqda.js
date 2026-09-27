'use strict';

module.exports = {
    generation_loaded(ui) {
        const contextContainer = ui.introductionControl.$el[0];
        let textarea = contextContainer.querySelector('textarea');
        if (!textarea) {
            textarea = document.createElement('textarea');
            textarea.rows = 5;
            textarea.setAttribute('aria-label', 'Interpretation context');
            textarea.style.width = '100%';
            textarea.style.minHeight = '110px';
            textarea.style.boxSizing = 'border-box';
            textarea.style.resize = 'vertical';
            textarea.style.font = 'inherit';
            textarea.style.display = 'block';
            contextContainer.appendChild(textarea);

            const introductionValue = () => {
                const value = ui.introduction.value();
                return value === null || value === undefined ? '' : String(value);
            };
            textarea.value = introductionValue();

            textarea.addEventListener('change', () => {
                if (textarea.value !== introductionValue())
                    ui.introduction.setValue(textarea.value);
            });
            ui.introduction.on(ui.introduction.getTrigger('value'), () => {
                const value = introductionValue();
                if (textarea.value !== value)
                    textarea.value = value;
            });
        }

        const container = ui.generationControl.$el[0];
        if (container.querySelector('button'))
            return;

        const button = document.createElement('button');
        button.type = 'button';
        button.className = 'jmv-action-button';
        button.textContent = 'Generate interpretation';
        container.appendChild(button);

        const update = () => {
            const attributes = ui.attributes.value();
            button.disabled = !(ui.product.value() && ui.panelist.value() &&
                attributes && attributes.length > 0);
        };
        for (const control of [ui.product, ui.panelist, ui.attributes])
            control.on(control.getTrigger('value'), update);
        update();

        button.addEventListener('click', () => {
            update();
            if (button.disabled)
                return;
            const words = window.crypto.getRandomValues(new Uint32Array(4));
            const id = Array.from(words, word => word.toString(16).padStart(8, '0')).join('');
            ui.generationRequest.setValue(id);
        });
    }
};

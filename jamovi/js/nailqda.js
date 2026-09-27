'use strict';

module.exports = {
    generation_loaded(ui) {
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

document.addEventListener('DOMContentLoaded', function () {
  const buttons = document.querySelectorAll('.custom-action');

  buttons.forEach(button => {
    button.addEventListener('click', function () {
      const groupId = button.getAttribute('data-id');

      const groupButtons = document.querySelectorAll(`.custom-action[data-id="${groupId}"]`);
      groupButtons.forEach(groupButton => {
        if (!groupButton.classList.contains('checked')) {
          groupButton.innerHTML += ' &#x2705;';
          groupButton.classList.add('checked');
        }
      });
    });
  });

  // Add file input reset handler
  Shiny.addCustomMessageHandler('resetFileInput', function(message) {
    document.getElementById(message.id).value = null;
  });
});

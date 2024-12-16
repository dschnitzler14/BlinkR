document.addEventListener('DOMContentLoaded', function () {
  const buttons = document.querySelectorAll('.custom-action');

  buttons.forEach(button => {
    button.addEventListener('click', function () {
      const groupId = button.getAttribute('data-id');

      // Select all buttons in the same group
      const groupButtons = document.querySelectorAll(`.custom-action[data-id="${groupId}"]`);

      groupButtons.forEach(groupButton => {
        // Add the checkmark only if not already added
        if (!groupButton.classList.contains('checked')) {
          groupButton.innerHTML += ' &#x2705;'; // Add a single checkmark
          groupButton.classList.add('checked'); // Mark as checked
        }
      });
    });
  });
});

document.addEventListener('DOMContentLoaded', function () {
  document.body.addEventListener('click', function (event) {
    if (event.target.classList.contains('custom-action')) {
      const button = event.target;
      const groupId = button.getAttribute('data-id');

      const groupButtons = document.querySelectorAll(`.custom-action[data-id="${groupId}"]`);
      groupButtons.forEach(groupButton => {
        if (!groupButton.classList.contains('checked')) {
          groupButton.innerHTML += ' ✅'; // Unicode checkmark
          groupButton.classList.add('checked');
        }
      });
    }
  });
});

// Ensure Shiny elements work when dynamically inserted
document.addEventListener('shiny:bound', function () {
  document.body.addEventListener('click', function (event) {
    if (event.target.classList.contains('custom-action')) {
      const button = event.target;
      const groupId = button.getAttribute('data-id');

      const groupButtons = document.querySelectorAll(`.custom-action[data-id="${groupId}"]`);
      groupButtons.forEach(groupButton => {
        if (!groupButton.classList.contains('checked')) {
          groupButton.innerHTML += ' ✅'; // Unicode checkmark
          groupButton.classList.add('checked');
        }
      });
    }
  });
});


// Run on page load for initially rendered elements
document.addEventListener('DOMContentLoaded', addCheckmarkToButtons);

// Run whenever new elements are bound by Shiny
document.addEventListener('shiny:bound', addCheckmarkToButtons);


  // Add file input reset handler
  Shiny.addCustomMessageHandler('resetFileInput', function(message) {
    document.getElementById(message.id).value = null;
  });
});

function collapseBox(boxid) {
  $('#' + boxid).find('[data-widget="collapse"]').click();
}

Shiny.addCustomMessageHandler("copyToClipboard", function(message) {
  // Use the modern Async Clipboard API
  navigator.clipboard.writeText(message).then(
    function() {
      // success callback
      alert("Copied citation to clipboard!");
    },
    function(err) {
      // error callback
      console.error("Async: Could not copy text: ", err);
      alert("Error copying to clipboard.");
    }
  );
});

$(document).ready(function() {
    $(".clickable-box .box-header").on("click", function(event) {
        // Prevent clicking the small collapse button from triggering twice
        if ($(event.target).closest(".btn-box-tool").length > 0) {
            return;
        }

        var box = $(this).closest(".box");
        var collapseButton = box.find("[data-widget='collapse']"); // Find the actual collapse button

        collapseButton.trigger("click"); // Simulate a click on the built-in collapse button
    });
});


HTMLWidgets.widget({

  name: 'TnT',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        // Code to render the widget
        var tot = eval(x.tntdef);
        tot(el);
        tot.start();
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
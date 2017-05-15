HTMLWidgets.widget({

  name: 'TnT',

  type: 'output',

  factory: function(el, width, height) {

    // Define shared variables for this instance
    var tntins = null;

    return {

      renderValue: function(x) {

        // Code to render the widget
        tntins = eval(x.tntdef);
        tntins(el);
        tntins.start();

        // TODO: In the future, we may implement interface to add/update data to
        //       an existing tnt instance (with shiny), and with proper transition.

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
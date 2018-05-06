HTMLWidgets.widget({

    name: 'trackWidget',

    type: 'output',

    factory: function(el, width, height) {
        
        // Some util functions
        var tnr = tnt.r;
        
        // Define shared variables for this instance
        var tntins = null;
        var initial_width = width;
        
        return {
            
            renderValue: function(x) {
                
                // Code to render the widget    ----------------
                
                tntins = eval(x.tntdef);
                
                // Set the initial width
                tntins.width(initial_width);
                
                // Remove the old one
                // TODO: better approach ?
                for (var i = 0; i < el.childNodes.length; i++) {
                    el.removeChild(el.childNodes[i]);
                }
                
                
                tntins(el);
                tntins.start();
                
                // TODO: In the future, we may implement interface to add/update data to
                //       an existing tnt instance (with shiny), and with proper transition.
            },
            
            resize: function(width, height) {
                // TODO: code to re-render the widget with a new size
                tntins.width(width);
            }
        };
    }
});


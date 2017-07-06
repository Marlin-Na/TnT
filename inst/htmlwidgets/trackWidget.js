HTMLWidgets.widget({

    name: 'trackWidget',

    type: 'output',

    factory: function(el, width, height) {
        
        // Some util functions
        var tnr = {};
        tnr.add_index = function (data) {
            var d = data;
            for (var i = 0; i < data.length; i++) {
                d[i][".index."] = i;
            }
            return d;
        };
        
        tnr.range_data_retriever = function (data) {
            // TODO:  We may sort the data here and provide fast search
            var rangeData = data;
            
            // Return a closure as the data retriever
            var ans = function (loc) {
                var min = loc.from;
                var max = loc.to;
                var ans = [];
                for (var i = 0; i < rangeData.length; i++) {
                    var row = rangeData[i];
                    if (row.start <= max && row.end >= min) {
                        ans.push(row);
                    }
                }
                return ans;
            };
            return ans;
        };
        
        tnr.pos_data_retriever = function (data) {
            var posData = data;
            var ans = function (loc) {
                var min = loc.from;
                var max = loc.to;
                var ans = [];
                for (var i = 0; i < posData.length; i++) {
                    var row = posData[i];
                    if (row.pos <= max && row.pos >= min) {
                        ans.push(row);
                    }
                }
                return ans;
            };
            return ans;
        };
        
        
        // Define shared variables for this instance
        var tntins = null;
        var initial_width = width;
        
        return {
            
            renderValue: function(x) {
                
                // Code to render the widget    ----------------
                
                tntins = eval(x.tntdef);
                
                // Set the initial width
                tntins.width(initial_width);
                
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
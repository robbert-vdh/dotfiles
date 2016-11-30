if (!String.prototype.endsWith) {
    String.prototype.endsWith = function(suffix, position) {
        var str = this;
        
        if (position) {
            str = str.substring(position);
        }
        
        if (str.length < suffix.length) {
            return false;
        }
        
        return str.slice(0 - suffix.length) == suffix;
    };
}
Elm.Native.Vendor = Elm.Native.Vendor || {};
Elm.Native.Vendor.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Vendor = elm.Native.Vendor || {};
    if (elm.Native.Vendor.values) return elm.Native.Vendor.values;

    //  http://davidwalsh.name/vendor-prefix
    var styles = window.getComputedStyle(document.documentElement, '');
    var vendorPrefix = (Array.prototype.slice
        .call(styles)
        .join('')
        .match(/-(moz|webkit|ms)-/) || (styles.OLink === '' && ['', 'o'])
    )[1];

    return elm.Native.Vendor.values = { prefix: vendorPrefix };
};

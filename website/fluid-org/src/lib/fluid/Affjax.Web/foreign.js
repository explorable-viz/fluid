export const driver = {
  newXHR: function () {
    return new XMLHttpRequest();
  },
  fixupUrl: function (url) {
    return url || "/";
  }
};

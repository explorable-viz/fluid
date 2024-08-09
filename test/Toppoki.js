import puppeteer from "puppeteer";
export {puppeteer};

export function _launch(options) {
  return function() {
    return puppeteer.launch(options);
  };
}

export function _launchChromeAWS(chromium, options) {
  return function() {
    return chromium.puppeteer.launch(options);
  };
}

export function _newPage(browser) {
  return function() {
    return browser.newPage();
  };
}

export function _goto(url, page) {
  return function() {
    return page.goto(url);
  };
}

export function _close(browser) {
  return function() {
    return browser.close();
  };
}

export function _content(page) {
  return function() {
    return page.content();
  };
}

export function _screenshot(options, page) {
  return function() {
    return page.screenshot(options);
  };
}

export function _pdf(options, page) {
  return function() {
    return page.pdf(options);
  };
}

export function _on(event, callback, page) {
  return page.on(event, callback);
}

export function _pageWaitForSelector(selector, options, page) {
  return function() {
    return page.waitForSelector(selector, options);
  };
}

export function _focus(selector, page) {
  return function() {
    return page.focus(selector);
  };
}

export function _select(selector, string, page) {
  return function() {
    return page.select(selector, string);
  };
}

export function _type(selector, content, options, page) {
  return function() {
    return page.type(selector, content, options);
  };
}

export function _setViewport(viewport, page) {
  return function() {
    return page.setViewport(viewport);
  };
}

export function _click(selector, page) {
  return function() {
    return page.click(selector);
  };
}

export function _waitForNavigation(options, page) {
  return function() {
    return page.waitForNavigation(options);
  };
}

export function _getLocationHref(page) {
  return function() {
    return page.evaluate(function() {
      return window.location.href;
    });
  };
}

export function _unsafeEvaluateOnNewDocument(string, page) {
  return function() {
     return page.evaluateOnNewDocument(string);
  }
}

export function _unsafeEvaluateStringFunction(string, page) {
  return function() {
    return page.evaluate(string);
  };
}

export function _unsafePageEval(selector, fnStr, page) {
  return function() {
    return page.$eval(selector, eval(fnStr));
  };
}

export function _unsafePageEvalAll(selector, fnStr, page) {
  return function() {
    return page.$$eval(selector, eval(fnStr));
  };
}

export function _keyboardDown(string, options, page) {
  return function() {
    return page.keyboard.down(string, options);
  };
}

export function _keyboardPress(key, options, page) {
  return function() {
    return page.keyboard.press(key, options);
  };
}

export function _keyboardSendCharacter(char, page) {
  return function() {
    return page.keyboard.sendCharacter(char);
  };
}

export function _keyboardType(text, options, page) {
  return function() {
    return page.keyboard.type(text, options);
  };
}

export function _keyboardUp(string, options, page) {
  return function() {
    return page.keyboard.up(string, options);
  };
}

export function _setUserAgent(string, page) {
  return function() {
    return page.setUserAgent(string);
  };
}

export function _bringToFront(page) {
  return function() {
    return page.bringToFront();
  };
}

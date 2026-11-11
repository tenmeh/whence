# Open a shinylive app in real Safari with safaridriver, and report whether
# its outputs fill in. Usage: python3 safari-demo.py <url> [seconds] [name]
#
# The console of the page is captured by wrapping console.* after the page
# loads. Shinylive can reload the page once to register its service worker,
# so the wrapper is put back whenever it is gone. webR takes seconds to
# start, so its messages come later.
import json
import sys
import time

from selenium import webdriver

url = sys.argv[1]
seconds = int(sys.argv[2]) if len(sys.argv) > 2 else 240
name = sys.argv[3] if len(sys.argv) > 3 else "demo"

CAPTURE = """
window.__logs = [];
['error', 'log', 'warn'].forEach(function (kind) {
  var original = console[kind];
  console[kind] = function () {
    try {
      window.__logs.push(kind + ': ' + Array.prototype.join.call(arguments, ' ').slice(0, 300));
      if (window.__logs.length > 4000) { window.__logs.shift(); }
    } catch (e) {}
    original.apply(console, arguments);
  };
});
window.addEventListener('error', function (e) { window.__logs.push('onerror: ' + e.message); });
window.addEventListener('unhandledrejection', function (e) {
  window.__logs.push('unhandledrejection: ' + String(e.reason).slice(0, 300));
});
"""

STATE = """
var frame = document.querySelector('iframe');
var doc = frame && frame.contentDocument;
if (!doc) { return null; }
return {
  outputs: Array.prototype.map.call(doc.querySelectorAll('.shiny-bound-output'), function (e) {
    return e.id + '=' + e.textContent.trim().replace(/\\s+/g, ' ').slice(0, 25);
  }),
  recalculating: doc.querySelectorAll('.recalculating').length
};
"""

driver = webdriver.Safari()
driver.set_window_size(1280, 900)
driver.get(url)
driver.execute_script(CAPTURE)

start = time.time()
state = None
logs = []
while time.time() - start < seconds:
    time.sleep(2)
    try:
        if not driver.execute_script("return !!window.__logs;"):
            driver.execute_script(CAPTURE)
        state = driver.execute_script(STATE)
        logs = driver.execute_script("return window.__logs || [];")
    except Exception as e:  # The page can be busy while webR starts.
        state = {"error": str(e)[:200]}
    filled = bool(state and state.get("outputs")) and state.get("recalculating") == 0
    crashed = any("call stack" in line.lower() for line in logs)
    if filled or crashed:
        break

driver.save_screenshot("safari-" + name + ".png")
print(json.dumps({
    "url": url,
    "seconds": round(time.time() - start),
    "filled": filled,
    "crashed": crashed,
    "state": state,
    "logs": logs,
}, indent=1))
driver.quit()

/**
 * Author: Leonardo Domingues
 *
 * #Web panel display enhancer
 * Toggle between
 * 		- Resize (Original)
 * 		- Overlay -> Click
 * 		- Overlay -> Hover
 *
 * Transition included
 */
setTimeout(function wait() {
  var adr = document.querySelector(".toolbar-addressbar.toolbar");
  if (adr != null) {
    // Panel Overlay Toggle

    var switchS = document.getElementById("switch");
    var btnS = document.createElement("button");
    var svgS = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    var pathS = document.createElementNS("http://www.w3.org/2000/svg", "path");

    var circTarget =
      "d: path('M 13 13m -6, 0a 6,6 0 1,0 12,0a 6,6 0 1,0 -12,0 M 13 13m -4, 0a 4,4 0 1,0 8,0a 4,4 0 1,0 -8,0 M 13 13m -2, 0a 2,2 0 1,0 4,0a 2,2 0 1,0 -4,0'); fill-rule: evenodd";

    var circEye =
      "d: path('M 13 13m -5.5, 0a 5.5,5.5 0 1,0 11,0a 5.5,5.5 0 1,0 -11,0 M 13 13m -2, 0a 2,2 0 1,0 4,0a 2,2 0 1,0 -4,0'); fill-rule: evenodd";

    var cirdRing =
      "d: path('M 13 13m -4, 0a 4,4 0 1,0 8,0a 4,4 0 1,0 -8,0 M 13 13m -2, 0a 2,2 0 1,0 4,0a 2,2 0 1,0 -4,0'); fill-rule: evenodd";

    var csso_overlay =
      "/* Overlay */ " +
      "#panels-container { position: absolute; z-index: 1; top: 0px; bottom: 0;}" +
      "#panels-container.right {right: 0; }" +
      "/* Overlay - Sides */" +
      "#main.left #webview-container { margin-left: 0px;}" +
      "#main.right #webview-container { margin-right: 0px;} " +
      "/* Fullscreen */" +
      "#browser.fullscreen #webview-container { margin-left: 0; margin-right: 0; } " +
      "#browser.fullscreen #panels-container { position: relative; }";

    btnS.classList.add("preferences");
    btnS.id = "toggle";
    btnS.setAttribute("tabindex", "-1");
    svgS.setAttributeNS(null, "width", "26");
    svgS.setAttributeNS(null, "height", "26");
    svgS.setAttributeNS(null, "viewBox", "0 0 26 26");
    switchS.lastChild.style = "margin-top: 0px";
    switchS.insertBefore(btnS, switchS.lastChild);
    btnS.appendChild(svgS);
    svgS.appendChild(pathS);

    var styleS = document.createElement("style");
    styleS.type = "text/css";
    document.getElementsByTagName("head")[0].appendChild(styleS);

    var web_panel = document.getElementById("panels-container");

    var mode = 0;
    var panel = 0;
    var show_token;
    var hide_token;

    function overlay_click() {
      styleS.innerHTML = csso_overlay;
      btnS.setAttribute("title", "Sobreposição 2");
      pathS.style = circEye;
      mode = 1;
    }

    function overlay_hover() {
      styleS.innerHTML = csso_overlay;
      btnS.setAttribute("title", "Encaixe");
      pathS.style = circTarget;
      mode = 2;

      for (
        let index = 0;
        index < switchS.getElementsByTagName("button").length - 3;
        index++
      ) {
        switchS.getElementsByTagName("button")[index].onmouseover =
          function () {
            clearTimeout(show_token);
            show_token = setTimeout(function () {
              if (
                !switchS
                  .getElementsByTagName("button")
                  [index].getAttribute("class")
                  .includes("active")
              ) {
                switchS.getElementsByTagName("button")[index].click();
                panel = index;
              }
            }, 100);
          };
      }

      web_panel.onmouseleave = function () {
        hide_token = setTimeout(function () {
          if (
            switchS
              .getElementsByTagName("button")
              [panel].getAttribute("class")
              .includes("active")
          ) {
            switchS.getElementsByTagName("button")[panel].click();
          }
        }, 400);
      };

      web_panel.onmouseenter = function () {
        clearTimeout(hide_token);
      };
    }

    function original() {
      for (
        let index = 0;
        index < switchS.getElementsByTagName("button").length - 3;
        index++
      ) {
        switchS.getElementsByTagName("button")[index].onmouseover = "";
      }
      web_panel.onmouseleave = "";
      web_panel.onmouseenter = "";

      styleS.innerHTML = "";
      btnS.setAttribute("title", "Sobreposição");
      pathS.style = cirdRing;
      mode = 0;
    }

    // set startup (replace this function with 1 of the above)

    original(); /*overlay_click ();*/ /*overlay_hover ();*/

    // toggle logic
    document.getElementById("toggle").addEventListener("click", function () {
      /********************************************************/

      var curr_width = web_panel.style.width;

      if (curr_width != "34px") {
        //In
        web_panel.style.transition = "width 0.3s";
        web_panel.style.width = "34px";
        //Out
        setTimeout(function () {
          web_panel.style.width = curr_width;
        }, 350);

        setTimeout(function () {
          web_panel.style.transition = "";
        }, 700);
      }
      /********************************************************/

      if (mode == 0) {
        overlay_click();
      } else if (mode == 1) {
        overlay_hover();
      } else {
        original();
      }
      /********************************************************/
    });
  } else {
    setTimeout(wait, 300);
  }
}, 300);

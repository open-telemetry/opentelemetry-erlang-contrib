(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, { get: all[name], enumerable: true });
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
    // If the importer is in node compatibility mode or this is not an ESM
    // file that has been converted to a CommonJS file using a Babel-
    // compatible transform (i.e. "__esModule" has not been set), then set
    // "default" to the CommonJS "module.exports" for node compatibility.
    isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
    mod
  ));

  // node_modules/htmx.org/dist/htmx.min.js
  var require_htmx_min = __commonJS({
    "node_modules/htmx.org/dist/htmx.min.js"(exports, module) {
      (function(e2, t2) {
        if (typeof define === "function" && define.amd) {
          define([], t2);
        } else if (typeof module === "object" && module.exports) {
          module.exports = t2();
        } else {
          e2.htmx = e2.htmx || t2();
        }
      })(typeof self !== "undefined" ? self : exports, function() {
        return function() {
          "use strict";
          var z = { onLoad: t, process: Tt, on: le, off: ue, trigger: ie, ajax: dr, find: b, findAll: f, closest: d, values: function(e2, t2) {
            var r2 = Jt(e2, t2 || "post");
            return r2.values;
          }, remove: B, addClass: j, removeClass: n, toggleClass: U, takeClass: V, defineExtension: yr, removeExtension: br, logAll: F, logger: null, config: { historyEnabled: true, historyCacheSize: 10, refreshOnHistoryMiss: false, defaultSwapStyle: "innerHTML", defaultSwapDelay: 0, defaultSettleDelay: 20, includeIndicatorStyles: true, indicatorClass: "htmx-indicator", requestClass: "htmx-request", addedClass: "htmx-added", settlingClass: "htmx-settling", swappingClass: "htmx-swapping", allowEval: true, inlineScriptNonce: "", attributesToSettle: ["class", "style", "width", "height"], withCredentials: false, timeout: 0, wsReconnectDelay: "full-jitter", wsBinaryType: "blob", disableSelector: "[hx-disable], [data-hx-disable]", useTemplateFragments: false, scrollBehavior: "smooth", defaultFocusScroll: false, getCacheBusterParam: false, globalViewTransitions: false }, parseInterval: v, _: e, createEventSource: function(e2) {
            return new EventSource(e2, { withCredentials: true });
          }, createWebSocket: function(e2) {
            var t2 = new WebSocket(e2, []);
            t2.binaryType = z.config.wsBinaryType;
            return t2;
          }, version: "1.9.2" };
          var C = { addTriggerHandler: xt, bodyContains: ee, canAccessLocalStorage: D, filterValues: er, hasAttribute: q, getAttributeValue: G, getClosestMatch: c, getExpressionVars: fr, getHeaders: Qt, getInputValues: Jt, getInternalData: Y, getSwapSpecification: rr, getTriggerSpecs: ze, getTarget: de, makeFragment: l, mergeObjects: te, makeSettleInfo: S, oobSwap: me, selectAndSwap: Me, settleImmediately: Bt, shouldCancel: Ke, triggerEvent: ie, triggerErrorEvent: ne, withExtensions: w };
          var R = ["get", "post", "put", "delete", "patch"];
          var O = R.map(function(e2) {
            return "[hx-" + e2 + "], [data-hx-" + e2 + "]";
          }).join(", ");
          function v(e2) {
            if (e2 == void 0) {
              return void 0;
            }
            if (e2.slice(-2) == "ms") {
              return parseFloat(e2.slice(0, -2)) || void 0;
            }
            if (e2.slice(-1) == "s") {
              return parseFloat(e2.slice(0, -1)) * 1e3 || void 0;
            }
            if (e2.slice(-1) == "m") {
              return parseFloat(e2.slice(0, -1)) * 1e3 * 60 || void 0;
            }
            return parseFloat(e2) || void 0;
          }
          function $(e2, t2) {
            return e2.getAttribute && e2.getAttribute(t2);
          }
          function q(e2, t2) {
            return e2.hasAttribute && (e2.hasAttribute(t2) || e2.hasAttribute("data-" + t2));
          }
          function G(e2, t2) {
            return $(e2, t2) || $(e2, "data-" + t2);
          }
          function u(e2) {
            return e2.parentElement;
          }
          function J() {
            return document;
          }
          function c(e2, t2) {
            while (e2 && !t2(e2)) {
              e2 = u(e2);
            }
            return e2 ? e2 : null;
          }
          function T(e2, t2, r2) {
            var n2 = G(t2, r2);
            var i2 = G(t2, "hx-disinherit");
            if (e2 !== t2 && i2 && (i2 === "*" || i2.split(" ").indexOf(r2) >= 0)) {
              return "unset";
            } else {
              return n2;
            }
          }
          function Z(t2, r2) {
            var n2 = null;
            c(t2, function(e2) {
              return n2 = T(t2, e2, r2);
            });
            if (n2 !== "unset") {
              return n2;
            }
          }
          function h(e2, t2) {
            var r2 = e2.matches || e2.matchesSelector || e2.msMatchesSelector || e2.mozMatchesSelector || e2.webkitMatchesSelector || e2.oMatchesSelector;
            return r2 && r2.call(e2, t2);
          }
          function H(e2) {
            var t2 = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i;
            var r2 = t2.exec(e2);
            if (r2) {
              return r2[1].toLowerCase();
            } else {
              return "";
            }
          }
          function i(e2, t2) {
            var r2 = new DOMParser();
            var n2 = r2.parseFromString(e2, "text/html");
            var i2 = n2.body;
            while (t2 > 0) {
              t2--;
              i2 = i2.firstChild;
            }
            if (i2 == null) {
              i2 = J().createDocumentFragment();
            }
            return i2;
          }
          function L(e2) {
            return e2.match(/<body/);
          }
          function l(e2) {
            var t2 = !L(e2);
            if (z.config.useTemplateFragments && t2) {
              var r2 = i("<body><template>" + e2 + "</template></body>", 0);
              return r2.querySelector("template").content;
            } else {
              var n2 = H(e2);
              switch (n2) {
                case "thead":
                case "tbody":
                case "tfoot":
                case "colgroup":
                case "caption":
                  return i("<table>" + e2 + "</table>", 1);
                case "col":
                  return i("<table><colgroup>" + e2 + "</colgroup></table>", 2);
                case "tr":
                  return i("<table><tbody>" + e2 + "</tbody></table>", 2);
                case "td":
                case "th":
                  return i("<table><tbody><tr>" + e2 + "</tr></tbody></table>", 3);
                case "script":
                  return i("<div>" + e2 + "</div>", 1);
                default:
                  return i(e2, 0);
              }
            }
          }
          function K(e2) {
            if (e2) {
              e2();
            }
          }
          function A(e2, t2) {
            return Object.prototype.toString.call(e2) === "[object " + t2 + "]";
          }
          function N(e2) {
            return A(e2, "Function");
          }
          function I(e2) {
            return A(e2, "Object");
          }
          function Y(e2) {
            var t2 = "htmx-internal-data";
            var r2 = e2[t2];
            if (!r2) {
              r2 = e2[t2] = {};
            }
            return r2;
          }
          function k(e2) {
            var t2 = [];
            if (e2) {
              for (var r2 = 0; r2 < e2.length; r2++) {
                t2.push(e2[r2]);
              }
            }
            return t2;
          }
          function Q(e2, t2) {
            if (e2) {
              for (var r2 = 0; r2 < e2.length; r2++) {
                t2(e2[r2]);
              }
            }
          }
          function P(e2) {
            var t2 = e2.getBoundingClientRect();
            var r2 = t2.top;
            var n2 = t2.bottom;
            return r2 < window.innerHeight && n2 >= 0;
          }
          function ee(e2) {
            if (e2.getRootNode && e2.getRootNode() instanceof ShadowRoot) {
              return J().body.contains(e2.getRootNode().host);
            } else {
              return J().body.contains(e2);
            }
          }
          function M(e2) {
            return e2.trim().split(/\s+/);
          }
          function te(e2, t2) {
            for (var r2 in t2) {
              if (t2.hasOwnProperty(r2)) {
                e2[r2] = t2[r2];
              }
            }
            return e2;
          }
          function y(e2) {
            try {
              return JSON.parse(e2);
            } catch (e3) {
              x(e3);
              return null;
            }
          }
          function D() {
            var e2 = "htmx:localStorageTest";
            try {
              localStorage.setItem(e2, e2);
              localStorage.removeItem(e2);
              return true;
            } catch (e3) {
              return false;
            }
          }
          function X(t2) {
            try {
              var e2 = new URL(t2);
              if (e2) {
                t2 = e2.pathname + e2.search;
              }
              if (!t2.match("^/$")) {
                t2 = t2.replace(/\/+$/, "");
              }
              return t2;
            } catch (e3) {
              return t2;
            }
          }
          function e(e) {
            return sr(J().body, function() {
              return eval(e);
            });
          }
          function t(t2) {
            var e2 = z.on("htmx:load", function(e3) {
              t2(e3.detail.elt);
            });
            return e2;
          }
          function F() {
            z.logger = function(e2, t2, r2) {
              if (console) {
                console.log(t2, e2, r2);
              }
            };
          }
          function b(e2, t2) {
            if (t2) {
              return e2.querySelector(t2);
            } else {
              return b(J(), e2);
            }
          }
          function f(e2, t2) {
            if (t2) {
              return e2.querySelectorAll(t2);
            } else {
              return f(J(), e2);
            }
          }
          function B(e2, t2) {
            e2 = s(e2);
            if (t2) {
              setTimeout(function() {
                B(e2);
                e2 = null;
              }, t2);
            } else {
              e2.parentElement.removeChild(e2);
            }
          }
          function j(e2, t2, r2) {
            e2 = s(e2);
            if (r2) {
              setTimeout(function() {
                j(e2, t2);
                e2 = null;
              }, r2);
            } else {
              e2.classList && e2.classList.add(t2);
            }
          }
          function n(e2, t2, r2) {
            e2 = s(e2);
            if (r2) {
              setTimeout(function() {
                n(e2, t2);
                e2 = null;
              }, r2);
            } else {
              if (e2.classList) {
                e2.classList.remove(t2);
                if (e2.classList.length === 0) {
                  e2.removeAttribute("class");
                }
              }
            }
          }
          function U(e2, t2) {
            e2 = s(e2);
            e2.classList.toggle(t2);
          }
          function V(e2, t2) {
            e2 = s(e2);
            Q(e2.parentElement.children, function(e3) {
              n(e3, t2);
            });
            j(e2, t2);
          }
          function d(e2, t2) {
            e2 = s(e2);
            if (e2.closest) {
              return e2.closest(t2);
            } else {
              do {
                if (e2 == null || h(e2, t2)) {
                  return e2;
                }
              } while (e2 = e2 && u(e2));
              return null;
            }
          }
          function r(e2) {
            var t2 = e2.trim();
            if (t2.startsWith("<") && t2.endsWith("/>")) {
              return t2.substring(1, t2.length - 2);
            } else {
              return t2;
            }
          }
          function _(e2, t2) {
            if (t2.indexOf("closest ") === 0) {
              return [d(e2, r(t2.substr(8)))];
            } else if (t2.indexOf("find ") === 0) {
              return [b(e2, r(t2.substr(5)))];
            } else if (t2.indexOf("next ") === 0) {
              return [W(e2, r(t2.substr(5)))];
            } else if (t2.indexOf("previous ") === 0) {
              return [oe(e2, r(t2.substr(9)))];
            } else if (t2 === "document") {
              return [document];
            } else if (t2 === "window") {
              return [window];
            } else {
              return J().querySelectorAll(r(t2));
            }
          }
          var W = function(e2, t2) {
            var r2 = J().querySelectorAll(t2);
            for (var n2 = 0; n2 < r2.length; n2++) {
              var i2 = r2[n2];
              if (i2.compareDocumentPosition(e2) === Node.DOCUMENT_POSITION_PRECEDING) {
                return i2;
              }
            }
          };
          var oe = function(e2, t2) {
            var r2 = J().querySelectorAll(t2);
            for (var n2 = r2.length - 1; n2 >= 0; n2--) {
              var i2 = r2[n2];
              if (i2.compareDocumentPosition(e2) === Node.DOCUMENT_POSITION_FOLLOWING) {
                return i2;
              }
            }
          };
          function re(e2, t2) {
            if (t2) {
              return _(e2, t2)[0];
            } else {
              return _(J().body, e2)[0];
            }
          }
          function s(e2) {
            if (A(e2, "String")) {
              return b(e2);
            } else {
              return e2;
            }
          }
          function se(e2, t2, r2) {
            if (N(t2)) {
              return { target: J().body, event: e2, listener: t2 };
            } else {
              return { target: s(e2), event: t2, listener: r2 };
            }
          }
          function le(t2, r2, n2) {
            Sr(function() {
              var e3 = se(t2, r2, n2);
              e3.target.addEventListener(e3.event, e3.listener);
            });
            var e2 = N(r2);
            return e2 ? r2 : n2;
          }
          function ue(t2, r2, n2) {
            Sr(function() {
              var e2 = se(t2, r2, n2);
              e2.target.removeEventListener(e2.event, e2.listener);
            });
            return N(r2) ? r2 : n2;
          }
          var fe = J().createElement("output");
          function ce(e2, t2) {
            var r2 = Z(e2, t2);
            if (r2) {
              if (r2 === "this") {
                return [he(e2, t2)];
              } else {
                var n2 = _(e2, r2);
                if (n2.length === 0) {
                  x('The selector "' + r2 + '" on ' + t2 + " returned no matches!");
                  return [fe];
                } else {
                  return n2;
                }
              }
            }
          }
          function he(e2, t2) {
            return c(e2, function(e3) {
              return G(e3, t2) != null;
            });
          }
          function de(e2) {
            var t2 = Z(e2, "hx-target");
            if (t2) {
              if (t2 === "this") {
                return he(e2, "hx-target");
              } else {
                return re(e2, t2);
              }
            } else {
              var r2 = Y(e2);
              if (r2.boosted) {
                return J().body;
              } else {
                return e2;
              }
            }
          }
          function ve(e2) {
            var t2 = z.config.attributesToSettle;
            for (var r2 = 0; r2 < t2.length; r2++) {
              if (e2 === t2[r2]) {
                return true;
              }
            }
            return false;
          }
          function ge(t2, r2) {
            Q(t2.attributes, function(e2) {
              if (!r2.hasAttribute(e2.name) && ve(e2.name)) {
                t2.removeAttribute(e2.name);
              }
            });
            Q(r2.attributes, function(e2) {
              if (ve(e2.name)) {
                t2.setAttribute(e2.name, e2.value);
              }
            });
          }
          function pe(e2, t2) {
            var r2 = wr(t2);
            for (var n2 = 0; n2 < r2.length; n2++) {
              var i2 = r2[n2];
              try {
                if (i2.isInlineSwap(e2)) {
                  return true;
                }
              } catch (e3) {
                x(e3);
              }
            }
            return e2 === "outerHTML";
          }
          function me(e2, i2, a2) {
            var t2 = "#" + i2.id;
            var o2 = "outerHTML";
            if (e2 === "true") {
            } else if (e2.indexOf(":") > 0) {
              o2 = e2.substr(0, e2.indexOf(":"));
              t2 = e2.substr(e2.indexOf(":") + 1, e2.length);
            } else {
              o2 = e2;
            }
            var r2 = J().querySelectorAll(t2);
            if (r2) {
              Q(r2, function(e3) {
                var t3;
                var r3 = i2.cloneNode(true);
                t3 = J().createDocumentFragment();
                t3.appendChild(r3);
                if (!pe(o2, e3)) {
                  t3 = r3;
                }
                var n2 = { shouldSwap: true, target: e3, fragment: t3 };
                if (!ie(e3, "htmx:oobBeforeSwap", n2))
                  return;
                e3 = n2.target;
                if (n2["shouldSwap"]) {
                  ke(o2, e3, e3, t3, a2);
                }
                Q(a2.elts, function(e4) {
                  ie(e4, "htmx:oobAfterSwap", n2);
                });
              });
              i2.parentNode.removeChild(i2);
            } else {
              i2.parentNode.removeChild(i2);
              ne(J().body, "htmx:oobErrorNoTarget", { content: i2 });
            }
            return e2;
          }
          function xe(e2, t2, r2) {
            var n2 = Z(e2, "hx-select-oob");
            if (n2) {
              var i2 = n2.split(",");
              for (let e3 = 0; e3 < i2.length; e3++) {
                var a2 = i2[e3].split(":", 2);
                var o2 = a2[0].trim();
                if (o2.indexOf("#") === 0) {
                  o2 = o2.substring(1);
                }
                var s2 = a2[1] || "true";
                var l2 = t2.querySelector("#" + o2);
                if (l2) {
                  me(s2, l2, r2);
                }
              }
            }
            Q(f(t2, "[hx-swap-oob], [data-hx-swap-oob]"), function(e3) {
              var t3 = G(e3, "hx-swap-oob");
              if (t3 != null) {
                me(t3, e3, r2);
              }
            });
          }
          function ye(e2) {
            Q(f(e2, "[hx-preserve], [data-hx-preserve]"), function(e3) {
              var t2 = G(e3, "id");
              var r2 = J().getElementById(t2);
              if (r2 != null) {
                e3.parentNode.replaceChild(r2, e3);
              }
            });
          }
          function be(a2, e2, o2) {
            Q(e2.querySelectorAll("[id]"), function(e3) {
              if (e3.id && e3.id.length > 0) {
                var t2 = e3.id.replace("'", "\\'");
                var r2 = e3.tagName.replace(":", "\\:");
                var n2 = a2.querySelector(r2 + "[id='" + t2 + "']");
                if (n2 && n2 !== a2) {
                  var i2 = e3.cloneNode();
                  ge(e3, n2);
                  o2.tasks.push(function() {
                    ge(e3, i2);
                  });
                }
              }
            });
          }
          function we(e2) {
            return function() {
              n(e2, z.config.addedClass);
              Tt(e2);
              bt(e2);
              Se(e2);
              ie(e2, "htmx:load");
            };
          }
          function Se(e2) {
            var t2 = "[autofocus]";
            var r2 = h(e2, t2) ? e2 : e2.querySelector(t2);
            if (r2 != null) {
              r2.focus();
            }
          }
          function a(e2, t2, r2, n2) {
            be(e2, r2, n2);
            while (r2.childNodes.length > 0) {
              var i2 = r2.firstChild;
              j(i2, z.config.addedClass);
              e2.insertBefore(i2, t2);
              if (i2.nodeType !== Node.TEXT_NODE && i2.nodeType !== Node.COMMENT_NODE) {
                n2.tasks.push(we(i2));
              }
            }
          }
          function Ee(e2, t2) {
            var r2 = 0;
            while (r2 < e2.length) {
              t2 = (t2 << 5) - t2 + e2.charCodeAt(r2++) | 0;
            }
            return t2;
          }
          function Ce(e2) {
            var t2 = 0;
            if (e2.attributes) {
              for (var r2 = 0; r2 < e2.attributes.length; r2++) {
                var n2 = e2.attributes[r2];
                if (n2.value) {
                  t2 = Ee(n2.name, t2);
                  t2 = Ee(n2.value, t2);
                }
              }
            }
            return t2;
          }
          function Re(t2) {
            var r2 = Y(t2);
            if (r2.timeout) {
              clearTimeout(r2.timeout);
            }
            if (r2.webSocket) {
              r2.webSocket.close();
            }
            if (r2.sseEventSource) {
              r2.sseEventSource.close();
            }
            if (r2.listenerInfos) {
              Q(r2.listenerInfos, function(e2) {
                if (e2.on) {
                  e2.on.removeEventListener(e2.trigger, e2.listener);
                }
              });
            }
            if (r2.onHandlers) {
              for (let e2 = 0; e2 < r2.onHandlers.length; e2++) {
                const n2 = r2.onHandlers[e2];
                t2.removeEventListener(n2.name, n2.handler);
              }
            }
          }
          function o(e2) {
            ie(e2, "htmx:beforeCleanupElement");
            Re(e2);
            if (e2.children) {
              Q(e2.children, function(e3) {
                o(e3);
              });
            }
          }
          function Oe(e2, t2, r2) {
            if (e2.tagName === "BODY") {
              return Ne(e2, t2, r2);
            } else {
              var n2;
              var i2 = e2.previousSibling;
              a(u(e2), e2, t2, r2);
              if (i2 == null) {
                n2 = u(e2).firstChild;
              } else {
                n2 = i2.nextSibling;
              }
              Y(e2).replacedWith = n2;
              r2.elts = [];
              while (n2 && n2 !== e2) {
                if (n2.nodeType === Node.ELEMENT_NODE) {
                  r2.elts.push(n2);
                }
                n2 = n2.nextElementSibling;
              }
              o(e2);
              u(e2).removeChild(e2);
            }
          }
          function qe(e2, t2, r2) {
            return a(e2, e2.firstChild, t2, r2);
          }
          function Te(e2, t2, r2) {
            return a(u(e2), e2, t2, r2);
          }
          function He(e2, t2, r2) {
            return a(e2, null, t2, r2);
          }
          function Le(e2, t2, r2) {
            return a(u(e2), e2.nextSibling, t2, r2);
          }
          function Ae(e2, t2, r2) {
            o(e2);
            return u(e2).removeChild(e2);
          }
          function Ne(e2, t2, r2) {
            var n2 = e2.firstChild;
            a(e2, n2, t2, r2);
            if (n2) {
              while (n2.nextSibling) {
                o(n2.nextSibling);
                e2.removeChild(n2.nextSibling);
              }
              o(n2);
              e2.removeChild(n2);
            }
          }
          function Ie(e2, t2) {
            var r2 = Z(e2, "hx-select");
            if (r2) {
              var n2 = J().createDocumentFragment();
              Q(t2.querySelectorAll(r2), function(e3) {
                n2.appendChild(e3);
              });
              t2 = n2;
            }
            return t2;
          }
          function ke(e2, t2, r2, n2, i2) {
            switch (e2) {
              case "none":
                return;
              case "outerHTML":
                Oe(r2, n2, i2);
                return;
              case "afterbegin":
                qe(r2, n2, i2);
                return;
              case "beforebegin":
                Te(r2, n2, i2);
                return;
              case "beforeend":
                He(r2, n2, i2);
                return;
              case "afterend":
                Le(r2, n2, i2);
                return;
              case "delete":
                Ae(r2, n2, i2);
                return;
              default:
                var a2 = wr(t2);
                for (var o2 = 0; o2 < a2.length; o2++) {
                  var s2 = a2[o2];
                  try {
                    var l2 = s2.handleSwap(e2, r2, n2, i2);
                    if (l2) {
                      if (typeof l2.length !== "undefined") {
                        for (var u2 = 0; u2 < l2.length; u2++) {
                          var f2 = l2[u2];
                          if (f2.nodeType !== Node.TEXT_NODE && f2.nodeType !== Node.COMMENT_NODE) {
                            i2.tasks.push(we(f2));
                          }
                        }
                      }
                      return;
                    }
                  } catch (e3) {
                    x(e3);
                  }
                }
                if (e2 === "innerHTML") {
                  Ne(r2, n2, i2);
                } else {
                  ke(z.config.defaultSwapStyle, t2, r2, n2, i2);
                }
            }
          }
          function Pe(e2) {
            if (e2.indexOf("<title") > -1) {
              var t2 = e2.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim, "");
              var r2 = t2.match(/<title(\s[^>]*>|>)([\s\S]*?)<\/title>/im);
              if (r2) {
                return r2[2];
              }
            }
          }
          function Me(e2, t2, r2, n2, i2) {
            i2.title = Pe(n2);
            var a2 = l(n2);
            if (a2) {
              xe(r2, a2, i2);
              a2 = Ie(r2, a2);
              ye(a2);
              return ke(e2, r2, t2, a2, i2);
            }
          }
          function De(e2, t2, r2) {
            var n2 = e2.getResponseHeader(t2);
            if (n2.indexOf("{") === 0) {
              var i2 = y(n2);
              for (var a2 in i2) {
                if (i2.hasOwnProperty(a2)) {
                  var o2 = i2[a2];
                  if (!I(o2)) {
                    o2 = { value: o2 };
                  }
                  ie(r2, a2, o2);
                }
              }
            } else {
              ie(r2, n2, []);
            }
          }
          var Xe = /\s/;
          var g = /[\s,]/;
          var Fe = /[_$a-zA-Z]/;
          var Be = /[_$a-zA-Z0-9]/;
          var je = ['"', "'", "/"];
          var p = /[^\s]/;
          function Ue(e2) {
            var t2 = [];
            var r2 = 0;
            while (r2 < e2.length) {
              if (Fe.exec(e2.charAt(r2))) {
                var n2 = r2;
                while (Be.exec(e2.charAt(r2 + 1))) {
                  r2++;
                }
                t2.push(e2.substr(n2, r2 - n2 + 1));
              } else if (je.indexOf(e2.charAt(r2)) !== -1) {
                var i2 = e2.charAt(r2);
                var n2 = r2;
                r2++;
                while (r2 < e2.length && e2.charAt(r2) !== i2) {
                  if (e2.charAt(r2) === "\\") {
                    r2++;
                  }
                  r2++;
                }
                t2.push(e2.substr(n2, r2 - n2 + 1));
              } else {
                var a2 = e2.charAt(r2);
                t2.push(a2);
              }
              r2++;
            }
            return t2;
          }
          function Ve(e2, t2, r2) {
            return Fe.exec(e2.charAt(0)) && e2 !== "true" && e2 !== "false" && e2 !== "this" && e2 !== r2 && t2 !== ".";
          }
          function _e(e2, t2, r2) {
            if (t2[0] === "[") {
              t2.shift();
              var n2 = 1;
              var i2 = " return (function(" + r2 + "){ return (";
              var a2 = null;
              while (t2.length > 0) {
                var o2 = t2[0];
                if (o2 === "]") {
                  n2--;
                  if (n2 === 0) {
                    if (a2 === null) {
                      i2 = i2 + "true";
                    }
                    t2.shift();
                    i2 += ")})";
                    try {
                      var s2 = sr(e2, function() {
                        return Function(i2)();
                      }, function() {
                        return true;
                      });
                      s2.source = i2;
                      return s2;
                    } catch (e3) {
                      ne(J().body, "htmx:syntax:error", { error: e3, source: i2 });
                      return null;
                    }
                  }
                } else if (o2 === "[") {
                  n2++;
                }
                if (Ve(o2, a2, r2)) {
                  i2 += "((" + r2 + "." + o2 + ") ? (" + r2 + "." + o2 + ") : (window." + o2 + "))";
                } else {
                  i2 = i2 + o2;
                }
                a2 = t2.shift();
              }
            }
          }
          function m(e2, t2) {
            var r2 = "";
            while (e2.length > 0 && !e2[0].match(t2)) {
              r2 += e2.shift();
            }
            return r2;
          }
          var We = "input, textarea, select";
          function ze(e2) {
            var t2 = G(e2, "hx-trigger");
            var r2 = [];
            if (t2) {
              var n2 = Ue(t2);
              do {
                m(n2, p);
                var i2 = n2.length;
                var a2 = m(n2, /[,\[\s]/);
                if (a2 !== "") {
                  if (a2 === "every") {
                    var o2 = { trigger: "every" };
                    m(n2, p);
                    o2.pollInterval = v(m(n2, /[,\[\s]/));
                    m(n2, p);
                    var s2 = _e(e2, n2, "event");
                    if (s2) {
                      o2.eventFilter = s2;
                    }
                    r2.push(o2);
                  } else if (a2.indexOf("sse:") === 0) {
                    r2.push({ trigger: "sse", sseEvent: a2.substr(4) });
                  } else {
                    var l2 = { trigger: a2 };
                    var s2 = _e(e2, n2, "event");
                    if (s2) {
                      l2.eventFilter = s2;
                    }
                    while (n2.length > 0 && n2[0] !== ",") {
                      m(n2, p);
                      var u2 = n2.shift();
                      if (u2 === "changed") {
                        l2.changed = true;
                      } else if (u2 === "once") {
                        l2.once = true;
                      } else if (u2 === "consume") {
                        l2.consume = true;
                      } else if (u2 === "delay" && n2[0] === ":") {
                        n2.shift();
                        l2.delay = v(m(n2, g));
                      } else if (u2 === "from" && n2[0] === ":") {
                        n2.shift();
                        var f2 = m(n2, g);
                        if (f2 === "closest" || f2 === "find" || f2 === "next" || f2 === "previous") {
                          n2.shift();
                          f2 += " " + m(n2, g);
                        }
                        l2.from = f2;
                      } else if (u2 === "target" && n2[0] === ":") {
                        n2.shift();
                        l2.target = m(n2, g);
                      } else if (u2 === "throttle" && n2[0] === ":") {
                        n2.shift();
                        l2.throttle = v(m(n2, g));
                      } else if (u2 === "queue" && n2[0] === ":") {
                        n2.shift();
                        l2.queue = m(n2, g);
                      } else if ((u2 === "root" || u2 === "threshold") && n2[0] === ":") {
                        n2.shift();
                        l2[u2] = m(n2, g);
                      } else {
                        ne(e2, "htmx:syntax:error", { token: n2.shift() });
                      }
                    }
                    r2.push(l2);
                  }
                }
                if (n2.length === i2) {
                  ne(e2, "htmx:syntax:error", { token: n2.shift() });
                }
                m(n2, p);
              } while (n2[0] === "," && n2.shift());
            }
            if (r2.length > 0) {
              return r2;
            } else if (h(e2, "form")) {
              return [{ trigger: "submit" }];
            } else if (h(e2, 'input[type="button"]')) {
              return [{ trigger: "click" }];
            } else if (h(e2, We)) {
              return [{ trigger: "change" }];
            } else {
              return [{ trigger: "click" }];
            }
          }
          function $e(e2) {
            Y(e2).cancelled = true;
          }
          function Ge(e2, t2, r2) {
            var n2 = Y(e2);
            n2.timeout = setTimeout(function() {
              if (ee(e2) && n2.cancelled !== true) {
                if (!Qe(r2, Lt("hx:poll:trigger", { triggerSpec: r2, target: e2 }))) {
                  t2(e2);
                }
                Ge(e2, t2, r2);
              }
            }, r2.pollInterval);
          }
          function Je(e2) {
            return location.hostname === e2.hostname && $(e2, "href") && $(e2, "href").indexOf("#") !== 0;
          }
          function Ze(t2, r2, e2) {
            if (t2.tagName === "A" && Je(t2) && (t2.target === "" || t2.target === "_self") || t2.tagName === "FORM") {
              r2.boosted = true;
              var n2, i2;
              if (t2.tagName === "A") {
                n2 = "get";
                i2 = t2.href;
              } else {
                var a2 = $(t2, "method");
                n2 = a2 ? a2.toLowerCase() : "get";
                if (n2 === "get") {
                }
                i2 = $(t2, "action");
              }
              e2.forEach(function(e3) {
                et(t2, function(e4, t3) {
                  ae(n2, i2, e4, t3);
                }, r2, e3, true);
              });
            }
          }
          function Ke(e2, t2) {
            if (e2.type === "submit" || e2.type === "click") {
              if (t2.tagName === "FORM") {
                return true;
              }
              if (h(t2, 'input[type="submit"], button') && d(t2, "form") !== null) {
                return true;
              }
              if (t2.tagName === "A" && t2.href && (t2.getAttribute("href") === "#" || t2.getAttribute("href").indexOf("#") !== 0)) {
                return true;
              }
            }
            return false;
          }
          function Ye(e2, t2) {
            return Y(e2).boosted && e2.tagName === "A" && t2.type === "click" && (t2.ctrlKey || t2.metaKey);
          }
          function Qe(e2, t2) {
            var r2 = e2.eventFilter;
            if (r2) {
              try {
                return r2(t2) !== true;
              } catch (e3) {
                ne(J().body, "htmx:eventFilter:error", { error: e3, source: r2.source });
                return true;
              }
            }
            return false;
          }
          function et(i2, a2, e2, o2, s2) {
            var l2 = Y(i2);
            var t2;
            if (o2.from) {
              t2 = _(i2, o2.from);
            } else {
              t2 = [i2];
            }
            if (o2.changed) {
              l2.lastValue = i2.value;
            }
            Q(t2, function(r2) {
              var n2 = function(e3) {
                if (!ee(i2)) {
                  r2.removeEventListener(o2.trigger, n2);
                  return;
                }
                if (Ye(i2, e3)) {
                  return;
                }
                if (s2 || Ke(e3, i2)) {
                  e3.preventDefault();
                }
                if (Qe(o2, e3)) {
                  return;
                }
                var t3 = Y(e3);
                t3.triggerSpec = o2;
                if (t3.handledFor == null) {
                  t3.handledFor = [];
                }
                if (t3.handledFor.indexOf(i2) < 0) {
                  t3.handledFor.push(i2);
                  if (o2.consume) {
                    e3.stopPropagation();
                  }
                  if (o2.target && e3.target) {
                    if (!h(e3.target, o2.target)) {
                      return;
                    }
                  }
                  if (o2.once) {
                    if (l2.triggeredOnce) {
                      return;
                    } else {
                      l2.triggeredOnce = true;
                    }
                  }
                  if (o2.changed) {
                    if (l2.lastValue === i2.value) {
                      return;
                    } else {
                      l2.lastValue = i2.value;
                    }
                  }
                  if (l2.delayed) {
                    clearTimeout(l2.delayed);
                  }
                  if (l2.throttle) {
                    return;
                  }
                  if (o2.throttle) {
                    if (!l2.throttle) {
                      a2(i2, e3);
                      l2.throttle = setTimeout(function() {
                        l2.throttle = null;
                      }, o2.throttle);
                    }
                  } else if (o2.delay) {
                    l2.delayed = setTimeout(function() {
                      a2(i2, e3);
                    }, o2.delay);
                  } else {
                    ie(i2, "htmx:trigger");
                    a2(i2, e3);
                  }
                }
              };
              if (e2.listenerInfos == null) {
                e2.listenerInfos = [];
              }
              e2.listenerInfos.push({ trigger: o2.trigger, listener: n2, on: r2 });
              r2.addEventListener(o2.trigger, n2);
            });
          }
          var tt = false;
          var rt = null;
          function nt() {
            if (!rt) {
              rt = function() {
                tt = true;
              };
              window.addEventListener("scroll", rt);
              setInterval(function() {
                if (tt) {
                  tt = false;
                  Q(J().querySelectorAll("[hx-trigger='revealed'],[data-hx-trigger='revealed']"), function(e2) {
                    it(e2);
                  });
                }
              }, 200);
            }
          }
          function it(t2) {
            if (!q(t2, "data-hx-revealed") && P(t2)) {
              t2.setAttribute("data-hx-revealed", "true");
              var e2 = Y(t2);
              if (e2.initHash) {
                ie(t2, "revealed");
              } else {
                t2.addEventListener("htmx:afterProcessNode", function(e3) {
                  ie(t2, "revealed");
                }, { once: true });
              }
            }
          }
          function at(e2, t2, r2) {
            var n2 = M(r2);
            for (var i2 = 0; i2 < n2.length; i2++) {
              var a2 = n2[i2].split(/:(.+)/);
              if (a2[0] === "connect") {
                ot(e2, a2[1], 0);
              }
              if (a2[0] === "send") {
                lt(e2);
              }
            }
          }
          function ot(s2, r2, n2) {
            if (!ee(s2)) {
              return;
            }
            if (r2.indexOf("/") == 0) {
              var e2 = location.hostname + (location.port ? ":" + location.port : "");
              if (location.protocol == "https:") {
                r2 = "wss://" + e2 + r2;
              } else if (location.protocol == "http:") {
                r2 = "ws://" + e2 + r2;
              }
            }
            var t2 = z.createWebSocket(r2);
            t2.onerror = function(e3) {
              ne(s2, "htmx:wsError", { error: e3, socket: t2 });
              st(s2);
            };
            t2.onclose = function(e3) {
              if ([1006, 1012, 1013].indexOf(e3.code) >= 0) {
                var t3 = ut(n2);
                setTimeout(function() {
                  ot(s2, r2, n2 + 1);
                }, t3);
              }
            };
            t2.onopen = function(e3) {
              n2 = 0;
            };
            Y(s2).webSocket = t2;
            t2.addEventListener("message", function(e3) {
              if (st(s2)) {
                return;
              }
              var t3 = e3.data;
              w(s2, function(e4) {
                t3 = e4.transformResponse(t3, null, s2);
              });
              var r3 = S(s2);
              var n3 = l(t3);
              var i2 = k(n3.children);
              for (var a2 = 0; a2 < i2.length; a2++) {
                var o2 = i2[a2];
                me(G(o2, "hx-swap-oob") || "true", o2, r3);
              }
              Bt(r3.tasks);
            });
          }
          function st(e2) {
            if (!ee(e2)) {
              Y(e2).webSocket.close();
              return true;
            }
          }
          function lt(u2) {
            var f2 = c(u2, function(e2) {
              return Y(e2).webSocket != null;
            });
            if (f2) {
              u2.addEventListener(ze(u2)[0].trigger, function(e2) {
                var t2 = Y(f2).webSocket;
                var r2 = Qt(u2, f2);
                var n2 = Jt(u2, "post");
                var i2 = n2.errors;
                var a2 = n2.values;
                var o2 = fr(u2);
                var s2 = te(a2, o2);
                var l2 = er(s2, u2);
                l2["HEADERS"] = r2;
                if (i2 && i2.length > 0) {
                  ie(u2, "htmx:validation:halted", i2);
                  return;
                }
                t2.send(JSON.stringify(l2));
                if (Ke(e2, u2)) {
                  e2.preventDefault();
                }
              });
            } else {
              ne(u2, "htmx:noWebSocketSourceError");
            }
          }
          function ut(e2) {
            var t2 = z.config.wsReconnectDelay;
            if (typeof t2 === "function") {
              return t2(e2);
            }
            if (t2 === "full-jitter") {
              var r2 = Math.min(e2, 6);
              var n2 = 1e3 * Math.pow(2, r2);
              return n2 * Math.random();
            }
            x('htmx.config.wsReconnectDelay must either be a function or the string "full-jitter"');
          }
          function ft(e2, t2, r2) {
            var n2 = M(r2);
            for (var i2 = 0; i2 < n2.length; i2++) {
              var a2 = n2[i2].split(/:(.+)/);
              if (a2[0] === "connect") {
                ct(e2, a2[1]);
              }
              if (a2[0] === "swap") {
                ht(e2, a2[1]);
              }
            }
          }
          function ct(t2, e2) {
            var r2 = z.createEventSource(e2);
            r2.onerror = function(e3) {
              ne(t2, "htmx:sseError", { error: e3, source: r2 });
              vt(t2);
            };
            Y(t2).sseEventSource = r2;
          }
          function ht(a2, o2) {
            var s2 = c(a2, gt);
            if (s2) {
              var l2 = Y(s2).sseEventSource;
              var u2 = function(e2) {
                if (vt(s2)) {
                  l2.removeEventListener(o2, u2);
                  return;
                }
                var t2 = e2.data;
                w(a2, function(e3) {
                  t2 = e3.transformResponse(t2, null, a2);
                });
                var r2 = rr(a2);
                var n2 = de(a2);
                var i2 = S(a2);
                Me(r2.swapStyle, a2, n2, t2, i2);
                Bt(i2.tasks);
                ie(a2, "htmx:sseMessage", e2);
              };
              Y(a2).sseListener = u2;
              l2.addEventListener(o2, u2);
            } else {
              ne(a2, "htmx:noSSESourceError");
            }
          }
          function dt(e2, t2, r2) {
            var n2 = c(e2, gt);
            if (n2) {
              var i2 = Y(n2).sseEventSource;
              var a2 = function() {
                if (!vt(n2)) {
                  if (ee(e2)) {
                    t2(e2);
                  } else {
                    i2.removeEventListener(r2, a2);
                  }
                }
              };
              Y(e2).sseListener = a2;
              i2.addEventListener(r2, a2);
            } else {
              ne(e2, "htmx:noSSESourceError");
            }
          }
          function vt(e2) {
            if (!ee(e2)) {
              Y(e2).sseEventSource.close();
              return true;
            }
          }
          function gt(e2) {
            return Y(e2).sseEventSource != null;
          }
          function pt(e2, t2, r2, n2) {
            var i2 = function() {
              if (!r2.loaded) {
                r2.loaded = true;
                t2(e2);
              }
            };
            if (n2) {
              setTimeout(i2, n2);
            } else {
              i2();
            }
          }
          function mt(t2, i2, e2) {
            var a2 = false;
            Q(R, function(r2) {
              if (q(t2, "hx-" + r2)) {
                var n2 = G(t2, "hx-" + r2);
                a2 = true;
                i2.path = n2;
                i2.verb = r2;
                e2.forEach(function(e3) {
                  xt(t2, e3, i2, function(e4, t3) {
                    ae(r2, n2, e4, t3);
                  });
                });
              }
            });
            return a2;
          }
          function xt(n2, e2, t2, r2) {
            if (e2.sseEvent) {
              dt(n2, r2, e2.sseEvent);
            } else if (e2.trigger === "revealed") {
              nt();
              et(n2, r2, t2, e2);
              it(n2);
            } else if (e2.trigger === "intersect") {
              var i2 = {};
              if (e2.root) {
                i2.root = re(n2, e2.root);
              }
              if (e2.threshold) {
                i2.threshold = parseFloat(e2.threshold);
              }
              var a2 = new IntersectionObserver(function(e3) {
                for (var t3 = 0; t3 < e3.length; t3++) {
                  var r3 = e3[t3];
                  if (r3.isIntersecting) {
                    ie(n2, "intersect");
                    break;
                  }
                }
              }, i2);
              a2.observe(n2);
              et(n2, r2, t2, e2);
            } else if (e2.trigger === "load") {
              if (!Qe(e2, Lt("load", { elt: n2 }))) {
                pt(n2, r2, t2, e2.delay);
              }
            } else if (e2.pollInterval) {
              t2.polling = true;
              Ge(n2, r2, e2);
            } else {
              et(n2, r2, t2, e2);
            }
          }
          function yt(e2) {
            if (e2.type === "text/javascript" || e2.type === "module" || e2.type === "") {
              var t2 = J().createElement("script");
              Q(e2.attributes, function(e3) {
                t2.setAttribute(e3.name, e3.value);
              });
              t2.textContent = e2.textContent;
              t2.async = false;
              if (z.config.inlineScriptNonce) {
                t2.nonce = z.config.inlineScriptNonce;
              }
              var r2 = e2.parentElement;
              try {
                r2.insertBefore(t2, e2);
              } catch (e3) {
                x(e3);
              } finally {
                if (e2.parentElement) {
                  e2.parentElement.removeChild(e2);
                }
              }
            }
          }
          function bt(e2) {
            if (h(e2, "script")) {
              yt(e2);
            }
            Q(f(e2, "script"), function(e3) {
              yt(e3);
            });
          }
          function wt() {
            return document.querySelector("[hx-boost], [data-hx-boost]");
          }
          function St(e2) {
            if (e2.querySelectorAll) {
              var t2 = wt() ? ", a, form" : "";
              var r2 = e2.querySelectorAll(O + t2 + ", [hx-sse], [data-hx-sse], [hx-ws], [data-hx-ws], [hx-ext], [data-hx-ext], [hx-trigger], [data-hx-trigger], [hx-on], [data-hx-on]");
              return r2;
            } else {
              return [];
            }
          }
          function Et(n2) {
            var e2 = function(e3) {
              var t2 = d(e3.target, "button, input[type='submit']");
              if (t2 !== null) {
                var r2 = Y(n2);
                r2.lastButtonClicked = t2;
              }
            };
            n2.addEventListener("click", e2);
            n2.addEventListener("focusin", e2);
            n2.addEventListener("focusout", function(e3) {
              var t2 = Y(n2);
              t2.lastButtonClicked = null;
            });
          }
          function Ct(e2) {
            var t2 = Ue(e2);
            var r2 = 0;
            for (let e3 = 0; e3 < t2.length; e3++) {
              const n2 = t2[e3];
              if (n2 === "{") {
                r2++;
              } else if (n2 === "}") {
                r2--;
              }
            }
            return r2;
          }
          function Rt(t2, e2, r2) {
            var n2 = Y(t2);
            n2.onHandlers = [];
            var i2 = new Function("event", r2 + "; return;");
            var a2 = t2.addEventListener(e2, function(e3) {
              return i2.call(t2, e3);
            });
            n2.onHandlers.push({ event: e2, listener: a2 });
            return { nodeData: n2, code: r2, func: i2, listener: a2 };
          }
          function Ot(e2) {
            var t2 = G(e2, "hx-on");
            if (t2) {
              var r2 = {};
              var n2 = t2.split("\n");
              var i2 = null;
              var a2 = 0;
              while (n2.length > 0) {
                var o2 = n2.shift();
                var s2 = o2.match(/^\s*([a-zA-Z:\-]+:)(.*)/);
                if (a2 === 0 && s2) {
                  o2.split(":");
                  i2 = s2[1].slice(0, -1);
                  r2[i2] = s2[2];
                } else {
                  r2[i2] += o2;
                }
                a2 += Ct(o2);
              }
              for (var l2 in r2) {
                Rt(e2, l2, r2[l2]);
              }
            }
          }
          function qt(t2) {
            if (t2.closest && t2.closest(z.config.disableSelector)) {
              return;
            }
            var r2 = Y(t2);
            if (r2.initHash !== Ce(t2)) {
              r2.initHash = Ce(t2);
              Re(t2);
              Ot(t2);
              ie(t2, "htmx:beforeProcessNode");
              if (t2.value) {
                r2.lastValue = t2.value;
              }
              var e2 = ze(t2);
              var n2 = mt(t2, r2, e2);
              if (!n2) {
                if (Z(t2, "hx-boost") === "true") {
                  Ze(t2, r2, e2);
                } else if (q(t2, "hx-trigger")) {
                  e2.forEach(function(e3) {
                    xt(t2, e3, r2, function() {
                    });
                  });
                }
              }
              if (t2.tagName === "FORM") {
                Et(t2);
              }
              var i2 = G(t2, "hx-sse");
              if (i2) {
                ft(t2, r2, i2);
              }
              var a2 = G(t2, "hx-ws");
              if (a2) {
                at(t2, r2, a2);
              }
              ie(t2, "htmx:afterProcessNode");
            }
          }
          function Tt(e2) {
            e2 = s(e2);
            qt(e2);
            Q(St(e2), function(e3) {
              qt(e3);
            });
          }
          function Ht(e2) {
            return e2.replace(/([a-z0-9])([A-Z])/g, "$1-$2").toLowerCase();
          }
          function Lt(e2, t2) {
            var r2;
            if (window.CustomEvent && typeof window.CustomEvent === "function") {
              r2 = new CustomEvent(e2, { bubbles: true, cancelable: true, detail: t2 });
            } else {
              r2 = J().createEvent("CustomEvent");
              r2.initCustomEvent(e2, true, true, t2);
            }
            return r2;
          }
          function ne(e2, t2, r2) {
            ie(e2, t2, te({ error: t2 }, r2));
          }
          function At(e2) {
            return e2 === "htmx:afterProcessNode";
          }
          function w(e2, t2) {
            Q(wr(e2), function(e3) {
              try {
                t2(e3);
              } catch (e4) {
                x(e4);
              }
            });
          }
          function x(e2) {
            if (console.error) {
              console.error(e2);
            } else if (console.log) {
              console.log("ERROR: ", e2);
            }
          }
          function ie(e2, t2, r2) {
            e2 = s(e2);
            if (r2 == null) {
              r2 = {};
            }
            r2["elt"] = e2;
            var n2 = Lt(t2, r2);
            if (z.logger && !At(t2)) {
              z.logger(e2, t2, r2);
            }
            if (r2.error) {
              x(r2.error);
              ie(e2, "htmx:error", { errorInfo: r2 });
            }
            var i2 = e2.dispatchEvent(n2);
            var a2 = Ht(t2);
            if (i2 && a2 !== t2) {
              var o2 = Lt(a2, n2.detail);
              i2 = i2 && e2.dispatchEvent(o2);
            }
            w(e2, function(e3) {
              i2 = i2 && e3.onEvent(t2, n2) !== false;
            });
            return i2;
          }
          var Nt = location.pathname + location.search;
          function It() {
            var e2 = J().querySelector("[hx-history-elt],[data-hx-history-elt]");
            return e2 || J().body;
          }
          function kt(e2, t2, r2, n2) {
            if (!D()) {
              return;
            }
            e2 = X(e2);
            var i2 = y(localStorage.getItem("htmx-history-cache")) || [];
            for (var a2 = 0; a2 < i2.length; a2++) {
              if (i2[a2].url === e2) {
                i2.splice(a2, 1);
                break;
              }
            }
            var o2 = { url: e2, content: t2, title: r2, scroll: n2 };
            ie(J().body, "htmx:historyItemCreated", { item: o2, cache: i2 });
            i2.push(o2);
            while (i2.length > z.config.historyCacheSize) {
              i2.shift();
            }
            while (i2.length > 0) {
              try {
                localStorage.setItem("htmx-history-cache", JSON.stringify(i2));
                break;
              } catch (e3) {
                ne(J().body, "htmx:historyCacheError", { cause: e3, cache: i2 });
                i2.shift();
              }
            }
          }
          function Pt(e2) {
            if (!D()) {
              return null;
            }
            e2 = X(e2);
            var t2 = y(localStorage.getItem("htmx-history-cache")) || [];
            for (var r2 = 0; r2 < t2.length; r2++) {
              if (t2[r2].url === e2) {
                return t2[r2];
              }
            }
            return null;
          }
          function Mt(e2) {
            var t2 = z.config.requestClass;
            var r2 = e2.cloneNode(true);
            Q(f(r2, "." + t2), function(e3) {
              n(e3, t2);
            });
            return r2.innerHTML;
          }
          function Dt() {
            var e2 = It();
            var t2 = Nt || location.pathname + location.search;
            var r2 = J().querySelector('[hx-history="false" i],[data-hx-history="false" i]');
            if (!r2) {
              ie(J().body, "htmx:beforeHistorySave", { path: t2, historyElt: e2 });
              kt(t2, Mt(e2), J().title, window.scrollY);
            }
            if (z.config.historyEnabled)
              history.replaceState({ htmx: true }, J().title, window.location.href);
          }
          function Xt(e2) {
            if (z.config.getCacheBusterParam) {
              e2 = e2.replace(/org\.htmx\.cache-buster=[^&]*&?/, "");
              if (e2.endsWith("&") || e2.endsWith("?")) {
                e2 = e2.slice(0, -1);
              }
            }
            if (z.config.historyEnabled) {
              history.pushState({ htmx: true }, "", e2);
            }
            Nt = e2;
          }
          function Ft(e2) {
            if (z.config.historyEnabled)
              history.replaceState({ htmx: true }, "", e2);
            Nt = e2;
          }
          function Bt(e2) {
            Q(e2, function(e3) {
              e3.call();
            });
          }
          function jt(a2) {
            var e2 = new XMLHttpRequest();
            var o2 = { path: a2, xhr: e2 };
            ie(J().body, "htmx:historyCacheMiss", o2);
            e2.open("GET", a2, true);
            e2.setRequestHeader("HX-History-Restore-Request", "true");
            e2.onload = function() {
              if (this.status >= 200 && this.status < 400) {
                ie(J().body, "htmx:historyCacheMissLoad", o2);
                var e3 = l(this.response);
                e3 = e3.querySelector("[hx-history-elt],[data-hx-history-elt]") || e3;
                var t2 = It();
                var r2 = S(t2);
                var n2 = Pe(this.response);
                if (n2) {
                  var i2 = b("title");
                  if (i2) {
                    i2.innerHTML = n2;
                  } else {
                    window.document.title = n2;
                  }
                }
                Ne(t2, e3, r2);
                Bt(r2.tasks);
                Nt = a2;
                ie(J().body, "htmx:historyRestore", { path: a2, cacheMiss: true, serverResponse: this.response });
              } else {
                ne(J().body, "htmx:historyCacheMissLoadError", o2);
              }
            };
            e2.send();
          }
          function Ut(e2) {
            Dt();
            e2 = e2 || location.pathname + location.search;
            var t2 = Pt(e2);
            if (t2) {
              var r2 = l(t2.content);
              var n2 = It();
              var i2 = S(n2);
              Ne(n2, r2, i2);
              Bt(i2.tasks);
              document.title = t2.title;
              window.scrollTo(0, t2.scroll);
              Nt = e2;
              ie(J().body, "htmx:historyRestore", { path: e2, item: t2 });
            } else {
              if (z.config.refreshOnHistoryMiss) {
                window.location.reload(true);
              } else {
                jt(e2);
              }
            }
          }
          function Vt(e2) {
            var t2 = ce(e2, "hx-indicator");
            if (t2 == null) {
              t2 = [e2];
            }
            Q(t2, function(e3) {
              var t3 = Y(e3);
              t3.requestCount = (t3.requestCount || 0) + 1;
              e3.classList["add"].call(e3.classList, z.config.requestClass);
            });
            return t2;
          }
          function _t(e2) {
            Q(e2, function(e3) {
              var t2 = Y(e3);
              t2.requestCount = (t2.requestCount || 0) - 1;
              if (t2.requestCount === 0) {
                e3.classList["remove"].call(e3.classList, z.config.requestClass);
              }
            });
          }
          function Wt(e2, t2) {
            for (var r2 = 0; r2 < e2.length; r2++) {
              var n2 = e2[r2];
              if (n2.isSameNode(t2)) {
                return true;
              }
            }
            return false;
          }
          function zt(e2) {
            if (e2.name === "" || e2.name == null || e2.disabled) {
              return false;
            }
            if (e2.type === "button" || e2.type === "submit" || e2.tagName === "image" || e2.tagName === "reset" || e2.tagName === "file") {
              return false;
            }
            if (e2.type === "checkbox" || e2.type === "radio") {
              return e2.checked;
            }
            return true;
          }
          function $t(t2, r2, n2, e2, i2) {
            if (e2 == null || Wt(t2, e2)) {
              return;
            } else {
              t2.push(e2);
            }
            if (zt(e2)) {
              var a2 = $(e2, "name");
              var o2 = e2.value;
              if (e2.multiple) {
                o2 = k(e2.querySelectorAll("option:checked")).map(function(e3) {
                  return e3.value;
                });
              }
              if (e2.files) {
                o2 = k(e2.files);
              }
              if (a2 != null && o2 != null) {
                var s2 = r2[a2];
                if (s2 !== void 0) {
                  if (Array.isArray(s2)) {
                    if (Array.isArray(o2)) {
                      r2[a2] = s2.concat(o2);
                    } else {
                      s2.push(o2);
                    }
                  } else {
                    if (Array.isArray(o2)) {
                      r2[a2] = [s2].concat(o2);
                    } else {
                      r2[a2] = [s2, o2];
                    }
                  }
                } else {
                  r2[a2] = o2;
                }
              }
              if (i2) {
                Gt(e2, n2);
              }
            }
            if (h(e2, "form")) {
              var l2 = e2.elements;
              Q(l2, function(e3) {
                $t(t2, r2, n2, e3, i2);
              });
            }
          }
          function Gt(e2, t2) {
            if (e2.willValidate) {
              ie(e2, "htmx:validation:validate");
              if (!e2.checkValidity()) {
                t2.push({ elt: e2, message: e2.validationMessage, validity: e2.validity });
                ie(e2, "htmx:validation:failed", { message: e2.validationMessage, validity: e2.validity });
              }
            }
          }
          function Jt(e2, t2) {
            var r2 = [];
            var n2 = {};
            var i2 = {};
            var a2 = [];
            var o2 = Y(e2);
            var s2 = h(e2, "form") && e2.noValidate !== true || G(e2, "hx-validate") === "true";
            if (o2.lastButtonClicked) {
              s2 = s2 && o2.lastButtonClicked.formNoValidate !== true;
            }
            if (t2 !== "get") {
              $t(r2, i2, a2, d(e2, "form"), s2);
            }
            $t(r2, n2, a2, e2, s2);
            if (o2.lastButtonClicked) {
              var l2 = $(o2.lastButtonClicked, "name");
              if (l2) {
                n2[l2] = o2.lastButtonClicked.value;
              }
            }
            var u2 = ce(e2, "hx-include");
            Q(u2, function(e3) {
              $t(r2, n2, a2, e3, s2);
              if (!h(e3, "form")) {
                Q(e3.querySelectorAll(We), function(e4) {
                  $t(r2, n2, a2, e4, s2);
                });
              }
            });
            n2 = te(n2, i2);
            return { errors: a2, values: n2 };
          }
          function Zt(e2, t2, r2) {
            if (e2 !== "") {
              e2 += "&";
            }
            if (String(r2) === "[object Object]") {
              r2 = JSON.stringify(r2);
            }
            var n2 = encodeURIComponent(r2);
            e2 += encodeURIComponent(t2) + "=" + n2;
            return e2;
          }
          function Kt(e2) {
            var t2 = "";
            for (var r2 in e2) {
              if (e2.hasOwnProperty(r2)) {
                var n2 = e2[r2];
                if (Array.isArray(n2)) {
                  Q(n2, function(e3) {
                    t2 = Zt(t2, r2, e3);
                  });
                } else {
                  t2 = Zt(t2, r2, n2);
                }
              }
            }
            return t2;
          }
          function Yt(e2) {
            var t2 = new FormData();
            for (var r2 in e2) {
              if (e2.hasOwnProperty(r2)) {
                var n2 = e2[r2];
                if (Array.isArray(n2)) {
                  Q(n2, function(e3) {
                    t2.append(r2, e3);
                  });
                } else {
                  t2.append(r2, n2);
                }
              }
            }
            return t2;
          }
          function Qt(e2, t2, r2) {
            var n2 = { "HX-Request": "true", "HX-Trigger": $(e2, "id"), "HX-Trigger-Name": $(e2, "name"), "HX-Target": G(t2, "id"), "HX-Current-URL": J().location.href };
            or(e2, "hx-headers", false, n2);
            if (r2 !== void 0) {
              n2["HX-Prompt"] = r2;
            }
            if (Y(e2).boosted) {
              n2["HX-Boosted"] = "true";
            }
            return n2;
          }
          function er(t2, e2) {
            var r2 = Z(e2, "hx-params");
            if (r2) {
              if (r2 === "none") {
                return {};
              } else if (r2 === "*") {
                return t2;
              } else if (r2.indexOf("not ") === 0) {
                Q(r2.substr(4).split(","), function(e3) {
                  e3 = e3.trim();
                  delete t2[e3];
                });
                return t2;
              } else {
                var n2 = {};
                Q(r2.split(","), function(e3) {
                  e3 = e3.trim();
                  n2[e3] = t2[e3];
                });
                return n2;
              }
            } else {
              return t2;
            }
          }
          function tr(e2) {
            return $(e2, "href") && $(e2, "href").indexOf("#") >= 0;
          }
          function rr(e2, t2) {
            var r2 = t2 ? t2 : Z(e2, "hx-swap");
            var n2 = { swapStyle: Y(e2).boosted ? "innerHTML" : z.config.defaultSwapStyle, swapDelay: z.config.defaultSwapDelay, settleDelay: z.config.defaultSettleDelay };
            if (Y(e2).boosted && !tr(e2)) {
              n2["show"] = "top";
            }
            if (r2) {
              var i2 = M(r2);
              if (i2.length > 0) {
                n2["swapStyle"] = i2[0];
                for (var a2 = 1; a2 < i2.length; a2++) {
                  var o2 = i2[a2];
                  if (o2.indexOf("swap:") === 0) {
                    n2["swapDelay"] = v(o2.substr(5));
                  }
                  if (o2.indexOf("settle:") === 0) {
                    n2["settleDelay"] = v(o2.substr(7));
                  }
                  if (o2.indexOf("transition:") === 0) {
                    n2["transition"] = o2.substr(11) === "true";
                  }
                  if (o2.indexOf("scroll:") === 0) {
                    var s2 = o2.substr(7);
                    var l2 = s2.split(":");
                    var u2 = l2.pop();
                    var f2 = l2.length > 0 ? l2.join(":") : null;
                    n2["scroll"] = u2;
                    n2["scrollTarget"] = f2;
                  }
                  if (o2.indexOf("show:") === 0) {
                    var c2 = o2.substr(5);
                    var l2 = c2.split(":");
                    var h2 = l2.pop();
                    var f2 = l2.length > 0 ? l2.join(":") : null;
                    n2["show"] = h2;
                    n2["showTarget"] = f2;
                  }
                  if (o2.indexOf("focus-scroll:") === 0) {
                    var d2 = o2.substr("focus-scroll:".length);
                    n2["focusScroll"] = d2 == "true";
                  }
                }
              }
            }
            return n2;
          }
          function nr(e2) {
            return Z(e2, "hx-encoding") === "multipart/form-data" || h(e2, "form") && $(e2, "enctype") === "multipart/form-data";
          }
          function ir(t2, r2, n2) {
            var i2 = null;
            w(r2, function(e2) {
              if (i2 == null) {
                i2 = e2.encodeParameters(t2, n2, r2);
              }
            });
            if (i2 != null) {
              return i2;
            } else {
              if (nr(r2)) {
                return Yt(n2);
              } else {
                return Kt(n2);
              }
            }
          }
          function S(e2) {
            return { tasks: [], elts: [e2] };
          }
          function ar(e2, t2) {
            var r2 = e2[0];
            var n2 = e2[e2.length - 1];
            if (t2.scroll) {
              var i2 = null;
              if (t2.scrollTarget) {
                i2 = re(r2, t2.scrollTarget);
              }
              if (t2.scroll === "top" && (r2 || i2)) {
                i2 = i2 || r2;
                i2.scrollTop = 0;
              }
              if (t2.scroll === "bottom" && (n2 || i2)) {
                i2 = i2 || n2;
                i2.scrollTop = i2.scrollHeight;
              }
            }
            if (t2.show) {
              var i2 = null;
              if (t2.showTarget) {
                var a2 = t2.showTarget;
                if (t2.showTarget === "window") {
                  a2 = "body";
                }
                i2 = re(r2, a2);
              }
              if (t2.show === "top" && (r2 || i2)) {
                i2 = i2 || r2;
                i2.scrollIntoView({ block: "start", behavior: z.config.scrollBehavior });
              }
              if (t2.show === "bottom" && (n2 || i2)) {
                i2 = i2 || n2;
                i2.scrollIntoView({ block: "end", behavior: z.config.scrollBehavior });
              }
            }
          }
          function or(e2, t2, r2, n2) {
            if (n2 == null) {
              n2 = {};
            }
            if (e2 == null) {
              return n2;
            }
            var i2 = G(e2, t2);
            if (i2) {
              var a2 = i2.trim();
              var o2 = r2;
              if (a2 === "unset") {
                return null;
              }
              if (a2.indexOf("javascript:") === 0) {
                a2 = a2.substr(11);
                o2 = true;
              } else if (a2.indexOf("js:") === 0) {
                a2 = a2.substr(3);
                o2 = true;
              }
              if (a2.indexOf("{") !== 0) {
                a2 = "{" + a2 + "}";
              }
              var s2;
              if (o2) {
                s2 = sr(e2, function() {
                  return Function("return (" + a2 + ")")();
                }, {});
              } else {
                s2 = y(a2);
              }
              for (var l2 in s2) {
                if (s2.hasOwnProperty(l2)) {
                  if (n2[l2] == null) {
                    n2[l2] = s2[l2];
                  }
                }
              }
            }
            return or(u(e2), t2, r2, n2);
          }
          function sr(e2, t2, r2) {
            if (z.config.allowEval) {
              return t2();
            } else {
              ne(e2, "htmx:evalDisallowedError");
              return r2;
            }
          }
          function lr(e2, t2) {
            return or(e2, "hx-vars", true, t2);
          }
          function ur(e2, t2) {
            return or(e2, "hx-vals", false, t2);
          }
          function fr(e2) {
            return te(lr(e2), ur(e2));
          }
          function cr(t2, r2, n2) {
            if (n2 !== null) {
              try {
                t2.setRequestHeader(r2, n2);
              } catch (e2) {
                t2.setRequestHeader(r2, encodeURIComponent(n2));
                t2.setRequestHeader(r2 + "-URI-AutoEncoded", "true");
              }
            }
          }
          function hr(t2) {
            if (t2.responseURL && typeof URL !== "undefined") {
              try {
                var e2 = new URL(t2.responseURL);
                return e2.pathname + e2.search;
              } catch (e3) {
                ne(J().body, "htmx:badResponseUrl", { url: t2.responseURL });
              }
            }
          }
          function E(e2, t2) {
            return e2.getAllResponseHeaders().match(t2);
          }
          function dr(e2, t2, r2) {
            e2 = e2.toLowerCase();
            if (r2) {
              if (r2 instanceof Element || A(r2, "String")) {
                return ae(e2, t2, null, null, { targetOverride: s(r2), returnPromise: true });
              } else {
                return ae(e2, t2, s(r2.source), r2.event, { handler: r2.handler, headers: r2.headers, values: r2.values, targetOverride: s(r2.target), swapOverride: r2.swap, returnPromise: true });
              }
            } else {
              return ae(e2, t2, null, null, { returnPromise: true });
            }
          }
          function vr(e2) {
            var t2 = [];
            while (e2) {
              t2.push(e2);
              e2 = e2.parentElement;
            }
            return t2;
          }
          function ae(e2, t2, n2, r2, i2, M2) {
            var a2 = null;
            var o2 = null;
            i2 = i2 != null ? i2 : {};
            if (i2.returnPromise && typeof Promise !== "undefined") {
              var s2 = new Promise(function(e3, t3) {
                a2 = e3;
                o2 = t3;
              });
            }
            if (n2 == null) {
              n2 = J().body;
            }
            var D2 = i2.handler || pr;
            if (!ee(n2)) {
              return;
            }
            var l2 = i2.targetOverride || de(n2);
            if (l2 == null || l2 == fe) {
              ne(n2, "htmx:targetError", { target: G(n2, "hx-target") });
              return;
            }
            if (!M2) {
              var X2 = function() {
                return ae(e2, t2, n2, r2, i2, true);
              };
              var F2 = { target: l2, elt: n2, path: t2, verb: e2, triggeringEvent: r2, etc: i2, issueRequest: X2 };
              if (ie(n2, "htmx:confirm", F2) === false) {
                return;
              }
            }
            var u2 = n2;
            var f2 = Y(n2);
            var c2 = Z(n2, "hx-sync");
            var h2 = null;
            var d2 = false;
            if (c2) {
              var v2 = c2.split(":");
              var g2 = v2[0].trim();
              if (g2 === "this") {
                u2 = he(n2, "hx-sync");
              } else {
                u2 = re(n2, g2);
              }
              c2 = (v2[1] || "drop").trim();
              f2 = Y(u2);
              if (c2 === "drop" && f2.xhr && f2.abortable !== true) {
                return;
              } else if (c2 === "abort") {
                if (f2.xhr) {
                  return;
                } else {
                  d2 = true;
                }
              } else if (c2 === "replace") {
                ie(u2, "htmx:abort");
              } else if (c2.indexOf("queue") === 0) {
                var B2 = c2.split(" ");
                h2 = (B2[1] || "last").trim();
              }
            }
            if (f2.xhr) {
              if (f2.abortable) {
                ie(u2, "htmx:abort");
              } else {
                if (h2 == null) {
                  if (r2) {
                    var p2 = Y(r2);
                    if (p2 && p2.triggerSpec && p2.triggerSpec.queue) {
                      h2 = p2.triggerSpec.queue;
                    }
                  }
                  if (h2 == null) {
                    h2 = "last";
                  }
                }
                if (f2.queuedRequests == null) {
                  f2.queuedRequests = [];
                }
                if (h2 === "first" && f2.queuedRequests.length === 0) {
                  f2.queuedRequests.push(function() {
                    ae(e2, t2, n2, r2, i2);
                  });
                } else if (h2 === "all") {
                  f2.queuedRequests.push(function() {
                    ae(e2, t2, n2, r2, i2);
                  });
                } else if (h2 === "last") {
                  f2.queuedRequests = [];
                  f2.queuedRequests.push(function() {
                    ae(e2, t2, n2, r2, i2);
                  });
                }
                return;
              }
            }
            var m2 = new XMLHttpRequest();
            f2.xhr = m2;
            f2.abortable = d2;
            var x2 = function() {
              f2.xhr = null;
              f2.abortable = false;
              if (f2.queuedRequests != null && f2.queuedRequests.length > 0) {
                var e3 = f2.queuedRequests.shift();
                e3();
              }
            };
            var y2 = Z(n2, "hx-prompt");
            if (y2) {
              var b2 = prompt(y2);
              if (b2 === null || !ie(n2, "htmx:prompt", { prompt: b2, target: l2 })) {
                K(a2);
                x2();
                return s2;
              }
            }
            var w2 = Z(n2, "hx-confirm");
            if (w2) {
              if (!confirm(w2)) {
                K(a2);
                x2();
                return s2;
              }
            }
            var S2 = Qt(n2, l2, b2);
            if (i2.headers) {
              S2 = te(S2, i2.headers);
            }
            var E2 = Jt(n2, e2);
            var C2 = E2.errors;
            var R2 = E2.values;
            if (i2.values) {
              R2 = te(R2, i2.values);
            }
            var j2 = fr(n2);
            var O2 = te(R2, j2);
            var q2 = er(O2, n2);
            if (e2 !== "get" && !nr(n2)) {
              S2["Content-Type"] = "application/x-www-form-urlencoded";
            }
            if (z.config.getCacheBusterParam && e2 === "get") {
              q2["org.htmx.cache-buster"] = $(l2, "id") || "true";
            }
            if (t2 == null || t2 === "") {
              t2 = J().location.href;
            }
            var T2 = or(n2, "hx-request");
            var H2 = Y(n2).boosted;
            var L2 = { boosted: H2, parameters: q2, unfilteredParameters: O2, headers: S2, target: l2, verb: e2, errors: C2, withCredentials: i2.credentials || T2.credentials || z.config.withCredentials, timeout: i2.timeout || T2.timeout || z.config.timeout, path: t2, triggeringEvent: r2 };
            if (!ie(n2, "htmx:configRequest", L2)) {
              K(a2);
              x2();
              return s2;
            }
            t2 = L2.path;
            e2 = L2.verb;
            S2 = L2.headers;
            q2 = L2.parameters;
            C2 = L2.errors;
            if (C2 && C2.length > 0) {
              ie(n2, "htmx:validation:halted", L2);
              K(a2);
              x2();
              return s2;
            }
            var U2 = t2.split("#");
            var V2 = U2[0];
            var A2 = U2[1];
            var N2 = null;
            if (e2 === "get") {
              N2 = V2;
              var _2 = Object.keys(q2).length !== 0;
              if (_2) {
                if (N2.indexOf("?") < 0) {
                  N2 += "?";
                } else {
                  N2 += "&";
                }
                N2 += Kt(q2);
                if (A2) {
                  N2 += "#" + A2;
                }
              }
              m2.open("GET", N2, true);
            } else {
              m2.open(e2.toUpperCase(), t2, true);
            }
            m2.overrideMimeType("text/html");
            m2.withCredentials = L2.withCredentials;
            m2.timeout = L2.timeout;
            if (T2.noHeaders) {
            } else {
              for (var I2 in S2) {
                if (S2.hasOwnProperty(I2)) {
                  var W2 = S2[I2];
                  cr(m2, I2, W2);
                }
              }
            }
            var k2 = { xhr: m2, target: l2, requestConfig: L2, etc: i2, boosted: H2, pathInfo: { requestPath: t2, finalRequestPath: N2 || t2, anchor: A2 } };
            m2.onload = function() {
              try {
                var e3 = vr(n2);
                k2.pathInfo.responsePath = hr(m2);
                D2(n2, k2);
                _t(P2);
                ie(n2, "htmx:afterRequest", k2);
                ie(n2, "htmx:afterOnLoad", k2);
                if (!ee(n2)) {
                  var t3 = null;
                  while (e3.length > 0 && t3 == null) {
                    var r3 = e3.shift();
                    if (ee(r3)) {
                      t3 = r3;
                    }
                  }
                  if (t3) {
                    ie(t3, "htmx:afterRequest", k2);
                    ie(t3, "htmx:afterOnLoad", k2);
                  }
                }
                K(a2);
                x2();
              } catch (e4) {
                ne(n2, "htmx:onLoadError", te({ error: e4 }, k2));
                throw e4;
              }
            };
            m2.onerror = function() {
              _t(P2);
              ne(n2, "htmx:afterRequest", k2);
              ne(n2, "htmx:sendError", k2);
              K(o2);
              x2();
            };
            m2.onabort = function() {
              _t(P2);
              ne(n2, "htmx:afterRequest", k2);
              ne(n2, "htmx:sendAbort", k2);
              K(o2);
              x2();
            };
            m2.ontimeout = function() {
              _t(P2);
              ne(n2, "htmx:afterRequest", k2);
              ne(n2, "htmx:timeout", k2);
              K(o2);
              x2();
            };
            if (!ie(n2, "htmx:beforeRequest", k2)) {
              K(a2);
              x2();
              return s2;
            }
            var P2 = Vt(n2);
            Q(["loadstart", "loadend", "progress", "abort"], function(t3) {
              Q([m2, m2.upload], function(e3) {
                e3.addEventListener(t3, function(e4) {
                  ie(n2, "htmx:xhr:" + t3, { lengthComputable: e4.lengthComputable, loaded: e4.loaded, total: e4.total });
                });
              });
            });
            ie(n2, "htmx:beforeSend", k2);
            m2.send(e2 === "get" ? null : ir(m2, n2, q2));
            return s2;
          }
          function gr(e2, t2) {
            var r2 = t2.xhr;
            var n2 = null;
            var i2 = null;
            if (E(r2, /HX-Push:/i)) {
              n2 = r2.getResponseHeader("HX-Push");
              i2 = "push";
            } else if (E(r2, /HX-Push-Url:/i)) {
              n2 = r2.getResponseHeader("HX-Push-Url");
              i2 = "push";
            } else if (E(r2, /HX-Replace-Url:/i)) {
              n2 = r2.getResponseHeader("HX-Replace-Url");
              i2 = "replace";
            }
            if (n2) {
              if (n2 === "false") {
                return {};
              } else {
                return { type: i2, path: n2 };
              }
            }
            var a2 = t2.pathInfo.finalRequestPath;
            var o2 = t2.pathInfo.responsePath;
            var s2 = Z(e2, "hx-push-url");
            var l2 = Z(e2, "hx-replace-url");
            var u2 = Y(e2).boosted;
            var f2 = null;
            var c2 = null;
            if (s2) {
              f2 = "push";
              c2 = s2;
            } else if (l2) {
              f2 = "replace";
              c2 = l2;
            } else if (u2) {
              f2 = "push";
              c2 = o2 || a2;
            }
            if (c2) {
              if (c2 === "false") {
                return {};
              }
              if (c2 === "true") {
                c2 = o2 || a2;
              }
              if (t2.pathInfo.anchor && c2.indexOf("#") === -1) {
                c2 = c2 + "#" + t2.pathInfo.anchor;
              }
              return { type: f2, path: c2 };
            } else {
              return {};
            }
          }
          function pr(s2, l2) {
            var u2 = l2.xhr;
            var f2 = l2.target;
            var e2 = l2.etc;
            if (!ie(s2, "htmx:beforeOnLoad", l2))
              return;
            if (E(u2, /HX-Trigger:/i)) {
              De(u2, "HX-Trigger", s2);
            }
            if (E(u2, /HX-Location:/i)) {
              Dt();
              var t2 = u2.getResponseHeader("HX-Location");
              var c2;
              if (t2.indexOf("{") === 0) {
                c2 = y(t2);
                t2 = c2["path"];
                delete c2["path"];
              }
              dr("GET", t2, c2).then(function() {
                Xt(t2);
              });
              return;
            }
            if (E(u2, /HX-Redirect:/i)) {
              location.href = u2.getResponseHeader("HX-Redirect");
              return;
            }
            if (E(u2, /HX-Refresh:/i)) {
              if ("true" === u2.getResponseHeader("HX-Refresh")) {
                location.reload();
                return;
              }
            }
            if (E(u2, /HX-Retarget:/i)) {
              l2.target = J().querySelector(u2.getResponseHeader("HX-Retarget"));
            }
            var h2 = gr(s2, l2);
            var r2 = u2.status >= 200 && u2.status < 400 && u2.status !== 204;
            var d2 = u2.response;
            var n2 = u2.status >= 400;
            var i2 = te({ shouldSwap: r2, serverResponse: d2, isError: n2 }, l2);
            if (!ie(f2, "htmx:beforeSwap", i2))
              return;
            f2 = i2.target;
            d2 = i2.serverResponse;
            n2 = i2.isError;
            l2.target = f2;
            l2.failed = n2;
            l2.successful = !n2;
            if (i2.shouldSwap) {
              if (u2.status === 286) {
                $e(s2);
              }
              w(s2, function(e3) {
                d2 = e3.transformResponse(d2, u2, s2);
              });
              if (h2.type) {
                Dt();
              }
              var a2 = e2.swapOverride;
              if (E(u2, /HX-Reswap:/i)) {
                a2 = u2.getResponseHeader("HX-Reswap");
              }
              var c2 = rr(s2, a2);
              f2.classList.add(z.config.swappingClass);
              var v2 = null;
              var g2 = null;
              var o2 = function() {
                try {
                  var e3 = document.activeElement;
                  var t3 = {};
                  try {
                    t3 = { elt: e3, start: e3 ? e3.selectionStart : null, end: e3 ? e3.selectionEnd : null };
                  } catch (e4) {
                  }
                  var n3 = S(f2);
                  Me(c2.swapStyle, f2, s2, d2, n3);
                  if (t3.elt && !ee(t3.elt) && t3.elt.id) {
                    var r3 = document.getElementById(t3.elt.id);
                    var i3 = { preventScroll: c2.focusScroll !== void 0 ? !c2.focusScroll : !z.config.defaultFocusScroll };
                    if (r3) {
                      if (t3.start && r3.setSelectionRange) {
                        try {
                          r3.setSelectionRange(t3.start, t3.end);
                        } catch (e4) {
                        }
                      }
                      r3.focus(i3);
                    }
                  }
                  f2.classList.remove(z.config.swappingClass);
                  Q(n3.elts, function(e4) {
                    if (e4.classList) {
                      e4.classList.add(z.config.settlingClass);
                    }
                    ie(e4, "htmx:afterSwap", l2);
                  });
                  if (E(u2, /HX-Trigger-After-Swap:/i)) {
                    var a3 = s2;
                    if (!ee(s2)) {
                      a3 = J().body;
                    }
                    De(u2, "HX-Trigger-After-Swap", a3);
                  }
                  var o3 = function() {
                    Q(n3.tasks, function(e5) {
                      e5.call();
                    });
                    Q(n3.elts, function(e5) {
                      if (e5.classList) {
                        e5.classList.remove(z.config.settlingClass);
                      }
                      ie(e5, "htmx:afterSettle", l2);
                    });
                    if (h2.type) {
                      if (h2.type === "push") {
                        Xt(h2.path);
                        ie(J().body, "htmx:pushedIntoHistory", { path: h2.path });
                      } else {
                        Ft(h2.path);
                        ie(J().body, "htmx:replacedInHistory", { path: h2.path });
                      }
                    }
                    if (l2.pathInfo.anchor) {
                      var e4 = b("#" + l2.pathInfo.anchor);
                      if (e4) {
                        e4.scrollIntoView({ block: "start", behavior: "auto" });
                      }
                    }
                    if (n3.title) {
                      var t4 = b("title");
                      if (t4) {
                        t4.innerHTML = n3.title;
                      } else {
                        window.document.title = n3.title;
                      }
                    }
                    ar(n3.elts, c2);
                    if (E(u2, /HX-Trigger-After-Settle:/i)) {
                      var r4 = s2;
                      if (!ee(s2)) {
                        r4 = J().body;
                      }
                      De(u2, "HX-Trigger-After-Settle", r4);
                    }
                    K(v2);
                  };
                  if (c2.settleDelay > 0) {
                    setTimeout(o3, c2.settleDelay);
                  } else {
                    o3();
                  }
                } catch (e4) {
                  ne(s2, "htmx:swapError", l2);
                  K(g2);
                  throw e4;
                }
              };
              var p2 = z.config.globalViewTransitions;
              if (c2.hasOwnProperty("transition")) {
                p2 = c2.transition;
              }
              if (p2 && ie(s2, "htmx:beforeTransition", l2) && typeof Promise !== "undefined" && document.startViewTransition) {
                var m2 = new Promise(function(e3, t3) {
                  v2 = e3;
                  g2 = t3;
                });
                var x2 = o2;
                o2 = function() {
                  document.startViewTransition(function() {
                    x2();
                    return m2;
                  });
                };
              }
              if (c2.swapDelay > 0) {
                setTimeout(o2, c2.swapDelay);
              } else {
                o2();
              }
            }
            if (n2) {
              ne(s2, "htmx:responseError", te({ error: "Response Status Error Code " + u2.status + " from " + l2.pathInfo.requestPath }, l2));
            }
          }
          var mr = {};
          function xr() {
            return { init: function(e2) {
              return null;
            }, onEvent: function(e2, t2) {
              return true;
            }, transformResponse: function(e2, t2, r2) {
              return e2;
            }, isInlineSwap: function(e2) {
              return false;
            }, handleSwap: function(e2, t2, r2, n2) {
              return false;
            }, encodeParameters: function(e2, t2, r2) {
              return null;
            } };
          }
          function yr(e2, t2) {
            if (t2.init) {
              t2.init(C);
            }
            mr[e2] = te(xr(), t2);
          }
          function br(e2) {
            delete mr[e2];
          }
          function wr(e2, r2, n2) {
            if (e2 == void 0) {
              return r2;
            }
            if (r2 == void 0) {
              r2 = [];
            }
            if (n2 == void 0) {
              n2 = [];
            }
            var t2 = G(e2, "hx-ext");
            if (t2) {
              Q(t2.split(","), function(e3) {
                e3 = e3.replace(/ /g, "");
                if (e3.slice(0, 7) == "ignore:") {
                  n2.push(e3.slice(7));
                  return;
                }
                if (n2.indexOf(e3) < 0) {
                  var t3 = mr[e3];
                  if (t3 && r2.indexOf(t3) < 0) {
                    r2.push(t3);
                  }
                }
              });
            }
            return wr(u(e2), r2, n2);
          }
          function Sr(e2) {
            if (J().readyState !== "loading") {
              e2();
            } else {
              J().addEventListener("DOMContentLoaded", e2);
            }
          }
          function Er() {
            if (z.config.includeIndicatorStyles !== false) {
              J().head.insertAdjacentHTML("beforeend", "<style>                      ." + z.config.indicatorClass + "{opacity:0;transition: opacity 200ms ease-in;}                      ." + z.config.requestClass + " ." + z.config.indicatorClass + "{opacity:1}                      ." + z.config.requestClass + "." + z.config.indicatorClass + "{opacity:1}                    </style>");
            }
          }
          function Cr() {
            var e2 = J().querySelector('meta[name="htmx-config"]');
            if (e2) {
              return y(e2.content);
            } else {
              return null;
            }
          }
          function Rr() {
            var e2 = Cr();
            if (e2) {
              z.config = te(z.config, e2);
            }
          }
          Sr(function() {
            Rr();
            Er();
            var e2 = J().body;
            Tt(e2);
            var t2 = J().querySelectorAll("[hx-trigger='restored'],[data-hx-trigger='restored']");
            e2.addEventListener("htmx:abort", function(e3) {
              var t3 = e3.target;
              var r3 = Y(t3);
              if (r3 && r3.xhr) {
                r3.xhr.abort();
              }
            });
            var r2 = window.onpopstate;
            window.onpopstate = function(e3) {
              if (e3.state && e3.state.htmx) {
                Ut();
                Q(t2, function(e4) {
                  ie(e4, "htmx:restored", { document: J(), triggerEvent: ie });
                });
              } else {
                if (r2) {
                  r2(e3);
                }
              }
            };
            setTimeout(function() {
              ie(e2, "htmx:load", {});
              e2 = null;
            }, 0);
          });
          return z;
        }();
      });
    }
  });

  // node_modules/shimmer/index.js
  var require_shimmer = __commonJS({
    "node_modules/shimmer/index.js"(exports2, module2) {
      "use strict";
      function isFunction2(funktion) {
        return typeof funktion === "function";
      }
      var logger = console.error.bind(console);
      function defineProperty(obj, name, value) {
        var enumerable = !!obj[name] && obj.propertyIsEnumerable(name);
        Object.defineProperty(obj, name, {
          configurable: true,
          enumerable,
          writable: true,
          value
        });
      }
      function shimmer2(options) {
        if (options && options.logger) {
          if (!isFunction2(options.logger))
            logger("new logger isn't a function, not replacing");
          else
            logger = options.logger;
        }
      }
      function wrap2(nodule, name, wrapper) {
        if (!nodule || !nodule[name]) {
          logger("no original function " + name + " to wrap");
          return;
        }
        if (!wrapper) {
          logger("no wrapper function");
          logger(new Error().stack);
          return;
        }
        if (!isFunction2(nodule[name]) || !isFunction2(wrapper)) {
          logger("original object and wrapper must be functions");
          return;
        }
        var original = nodule[name];
        var wrapped = wrapper(original, name);
        defineProperty(wrapped, "__original", original);
        defineProperty(wrapped, "__unwrap", function() {
          if (nodule[name] === wrapped)
            defineProperty(nodule, name, original);
        });
        defineProperty(wrapped, "__wrapped", true);
        defineProperty(nodule, name, wrapped);
        return wrapped;
      }
      function massWrap2(nodules, names, wrapper) {
        if (!nodules) {
          logger("must provide one or more modules to patch");
          logger(new Error().stack);
          return;
        } else if (!Array.isArray(nodules)) {
          nodules = [nodules];
        }
        if (!(names && Array.isArray(names))) {
          logger("must provide one or more functions to wrap on modules");
          return;
        }
        nodules.forEach(function(nodule) {
          names.forEach(function(name) {
            wrap2(nodule, name, wrapper);
          });
        });
      }
      function unwrap2(nodule, name) {
        if (!nodule || !nodule[name]) {
          logger("no function to unwrap.");
          logger(new Error().stack);
          return;
        }
        if (!nodule[name].__unwrap) {
          logger("no original to unwrap to -- has " + name + " already been unwrapped?");
        } else {
          return nodule[name].__unwrap();
        }
      }
      function massUnwrap2(nodules, names) {
        if (!nodules) {
          logger("must provide one or more modules to patch");
          logger(new Error().stack);
          return;
        } else if (!Array.isArray(nodules)) {
          nodules = [nodules];
        }
        if (!(names && Array.isArray(names))) {
          logger("must provide one or more functions to unwrap on modules");
          return;
        }
        nodules.forEach(function(nodule) {
          names.forEach(function(name) {
            unwrap2(nodule, name);
          });
        });
      }
      shimmer2.wrap = wrap2;
      shimmer2.massWrap = massWrap2;
      shimmer2.unwrap = unwrap2;
      shimmer2.massUnwrap = massUnwrap2;
      module2.exports = shimmer2;
    }
  });

  // js/app.js
  var import_htmx = __toESM(require_htmx_min());

  // node_modules/@opentelemetry/api/build/esm/platform/browser/globalThis.js
  var _globalThis = typeof globalThis === "object" ? globalThis : typeof self === "object" ? self : typeof window === "object" ? window : typeof global === "object" ? global : {};

  // node_modules/@opentelemetry/api/build/esm/version.js
  var VERSION = "1.4.1";

  // node_modules/@opentelemetry/api/build/esm/internal/semver.js
  var re2 = /^(\d+)\.(\d+)\.(\d+)(-(.+))?$/;
  function _makeCompatibilityCheck(ownVersion) {
    var acceptedVersions = /* @__PURE__ */ new Set([ownVersion]);
    var rejectedVersions = /* @__PURE__ */ new Set();
    var myVersionMatch = ownVersion.match(re2);
    if (!myVersionMatch) {
      return function() {
        return false;
      };
    }
    var ownVersionParsed = {
      major: +myVersionMatch[1],
      minor: +myVersionMatch[2],
      patch: +myVersionMatch[3],
      prerelease: myVersionMatch[4]
    };
    if (ownVersionParsed.prerelease != null) {
      return function isExactmatch(globalVersion) {
        return globalVersion === ownVersion;
      };
    }
    function _reject(v2) {
      rejectedVersions.add(v2);
      return false;
    }
    function _accept(v2) {
      acceptedVersions.add(v2);
      return true;
    }
    return function isCompatible2(globalVersion) {
      if (acceptedVersions.has(globalVersion)) {
        return true;
      }
      if (rejectedVersions.has(globalVersion)) {
        return false;
      }
      var globalVersionMatch = globalVersion.match(re2);
      if (!globalVersionMatch) {
        return _reject(globalVersion);
      }
      var globalVersionParsed = {
        major: +globalVersionMatch[1],
        minor: +globalVersionMatch[2],
        patch: +globalVersionMatch[3],
        prerelease: globalVersionMatch[4]
      };
      if (globalVersionParsed.prerelease != null) {
        return _reject(globalVersion);
      }
      if (ownVersionParsed.major !== globalVersionParsed.major) {
        return _reject(globalVersion);
      }
      if (ownVersionParsed.major === 0) {
        if (ownVersionParsed.minor === globalVersionParsed.minor && ownVersionParsed.patch <= globalVersionParsed.patch) {
          return _accept(globalVersion);
        }
        return _reject(globalVersion);
      }
      if (ownVersionParsed.minor <= globalVersionParsed.minor) {
        return _accept(globalVersion);
      }
      return _reject(globalVersion);
    };
  }
  var isCompatible = _makeCompatibilityCheck(VERSION);

  // node_modules/@opentelemetry/api/build/esm/internal/global-utils.js
  var major = VERSION.split(".")[0];
  var GLOBAL_OPENTELEMETRY_API_KEY = Symbol.for("opentelemetry.js.api." + major);
  var _global = _globalThis;
  function registerGlobal(type, instance, diag3, allowOverride) {
    var _a2;
    if (allowOverride === void 0) {
      allowOverride = false;
    }
    var api = _global[GLOBAL_OPENTELEMETRY_API_KEY] = (_a2 = _global[GLOBAL_OPENTELEMETRY_API_KEY]) !== null && _a2 !== void 0 ? _a2 : {
      version: VERSION
    };
    if (!allowOverride && api[type]) {
      var err = new Error("@opentelemetry/api: Attempted duplicate registration of API: " + type);
      diag3.error(err.stack || err.message);
      return false;
    }
    if (api.version !== VERSION) {
      var err = new Error("@opentelemetry/api: Registration of version v" + api.version + " for " + type + " does not match previously registered API v" + VERSION);
      diag3.error(err.stack || err.message);
      return false;
    }
    api[type] = instance;
    diag3.debug("@opentelemetry/api: Registered a global for " + type + " v" + VERSION + ".");
    return true;
  }
  function getGlobal(type) {
    var _a2, _b;
    var globalVersion = (_a2 = _global[GLOBAL_OPENTELEMETRY_API_KEY]) === null || _a2 === void 0 ? void 0 : _a2.version;
    if (!globalVersion || !isCompatible(globalVersion)) {
      return;
    }
    return (_b = _global[GLOBAL_OPENTELEMETRY_API_KEY]) === null || _b === void 0 ? void 0 : _b[type];
  }
  function unregisterGlobal(type, diag3) {
    diag3.debug("@opentelemetry/api: Unregistering a global for " + type + " v" + VERSION + ".");
    var api = _global[GLOBAL_OPENTELEMETRY_API_KEY];
    if (api) {
      delete api[type];
    }
  }

  // node_modules/@opentelemetry/api/build/esm/diag/ComponentLogger.js
  var __read = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var DiagComponentLogger = (
    /** @class */
    function() {
      function DiagComponentLogger2(props) {
        this._namespace = props.namespace || "DiagComponentLogger";
      }
      DiagComponentLogger2.prototype.debug = function() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return logProxy("debug", this._namespace, args);
      };
      DiagComponentLogger2.prototype.error = function() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return logProxy("error", this._namespace, args);
      };
      DiagComponentLogger2.prototype.info = function() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return logProxy("info", this._namespace, args);
      };
      DiagComponentLogger2.prototype.warn = function() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return logProxy("warn", this._namespace, args);
      };
      DiagComponentLogger2.prototype.verbose = function() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        return logProxy("verbose", this._namespace, args);
      };
      return DiagComponentLogger2;
    }()
  );
  function logProxy(funcName, namespace, args) {
    var logger = getGlobal("diag");
    if (!logger) {
      return;
    }
    args.unshift(namespace);
    return logger[funcName].apply(logger, __spreadArray([], __read(args), false));
  }

  // node_modules/@opentelemetry/api/build/esm/diag/types.js
  var DiagLogLevel;
  (function(DiagLogLevel2) {
    DiagLogLevel2[DiagLogLevel2["NONE"] = 0] = "NONE";
    DiagLogLevel2[DiagLogLevel2["ERROR"] = 30] = "ERROR";
    DiagLogLevel2[DiagLogLevel2["WARN"] = 50] = "WARN";
    DiagLogLevel2[DiagLogLevel2["INFO"] = 60] = "INFO";
    DiagLogLevel2[DiagLogLevel2["DEBUG"] = 70] = "DEBUG";
    DiagLogLevel2[DiagLogLevel2["VERBOSE"] = 80] = "VERBOSE";
    DiagLogLevel2[DiagLogLevel2["ALL"] = 9999] = "ALL";
  })(DiagLogLevel || (DiagLogLevel = {}));

  // node_modules/@opentelemetry/api/build/esm/diag/internal/logLevelLogger.js
  function createLogLevelDiagLogger(maxLevel, logger) {
    if (maxLevel < DiagLogLevel.NONE) {
      maxLevel = DiagLogLevel.NONE;
    } else if (maxLevel > DiagLogLevel.ALL) {
      maxLevel = DiagLogLevel.ALL;
    }
    logger = logger || {};
    function _filterFunc(funcName, theLevel) {
      var theFunc = logger[funcName];
      if (typeof theFunc === "function" && maxLevel >= theLevel) {
        return theFunc.bind(logger);
      }
      return function() {
      };
    }
    return {
      error: _filterFunc("error", DiagLogLevel.ERROR),
      warn: _filterFunc("warn", DiagLogLevel.WARN),
      info: _filterFunc("info", DiagLogLevel.INFO),
      debug: _filterFunc("debug", DiagLogLevel.DEBUG),
      verbose: _filterFunc("verbose", DiagLogLevel.VERBOSE)
    };
  }

  // node_modules/@opentelemetry/api/build/esm/api/diag.js
  var __read2 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray2 = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var API_NAME = "diag";
  var DiagAPI = (
    /** @class */
    function() {
      function DiagAPI2() {
        function _logProxy(funcName) {
          return function() {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
              args[_i] = arguments[_i];
            }
            var logger = getGlobal("diag");
            if (!logger)
              return;
            return logger[funcName].apply(logger, __spreadArray2([], __read2(args), false));
          };
        }
        var self2 = this;
        var setLogger = function(logger, optionsOrLogLevel) {
          var _a2, _b, _c;
          if (optionsOrLogLevel === void 0) {
            optionsOrLogLevel = { logLevel: DiagLogLevel.INFO };
          }
          if (logger === self2) {
            var err = new Error("Cannot use diag as the logger for itself. Please use a DiagLogger implementation like ConsoleDiagLogger or a custom implementation");
            self2.error((_a2 = err.stack) !== null && _a2 !== void 0 ? _a2 : err.message);
            return false;
          }
          if (typeof optionsOrLogLevel === "number") {
            optionsOrLogLevel = {
              logLevel: optionsOrLogLevel
            };
          }
          var oldLogger = getGlobal("diag");
          var newLogger = createLogLevelDiagLogger((_b = optionsOrLogLevel.logLevel) !== null && _b !== void 0 ? _b : DiagLogLevel.INFO, logger);
          if (oldLogger && !optionsOrLogLevel.suppressOverrideMessage) {
            var stack = (_c = new Error().stack) !== null && _c !== void 0 ? _c : "<failed to generate stacktrace>";
            oldLogger.warn("Current logger will be overwritten from " + stack);
            newLogger.warn("Current logger will overwrite one already registered from " + stack);
          }
          return registerGlobal("diag", newLogger, self2, true);
        };
        self2.setLogger = setLogger;
        self2.disable = function() {
          unregisterGlobal(API_NAME, self2);
        };
        self2.createComponentLogger = function(options) {
          return new DiagComponentLogger(options);
        };
        self2.verbose = _logProxy("verbose");
        self2.debug = _logProxy("debug");
        self2.info = _logProxy("info");
        self2.warn = _logProxy("warn");
        self2.error = _logProxy("error");
      }
      DiagAPI2.instance = function() {
        if (!this._instance) {
          this._instance = new DiagAPI2();
        }
        return this._instance;
      };
      return DiagAPI2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/baggage/internal/baggage-impl.js
  var __read3 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __values = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var BaggageImpl = (
    /** @class */
    function() {
      function BaggageImpl2(entries) {
        this._entries = entries ? new Map(entries) : /* @__PURE__ */ new Map();
      }
      BaggageImpl2.prototype.getEntry = function(key) {
        var entry = this._entries.get(key);
        if (!entry) {
          return void 0;
        }
        return Object.assign({}, entry);
      };
      BaggageImpl2.prototype.getAllEntries = function() {
        return Array.from(this._entries.entries()).map(function(_a2) {
          var _b = __read3(_a2, 2), k2 = _b[0], v2 = _b[1];
          return [k2, v2];
        });
      };
      BaggageImpl2.prototype.setEntry = function(key, entry) {
        var newBaggage = new BaggageImpl2(this._entries);
        newBaggage._entries.set(key, entry);
        return newBaggage;
      };
      BaggageImpl2.prototype.removeEntry = function(key) {
        var newBaggage = new BaggageImpl2(this._entries);
        newBaggage._entries.delete(key);
        return newBaggage;
      };
      BaggageImpl2.prototype.removeEntries = function() {
        var e_1, _a2;
        var keys = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          keys[_i] = arguments[_i];
        }
        var newBaggage = new BaggageImpl2(this._entries);
        try {
          for (var keys_1 = __values(keys), keys_1_1 = keys_1.next(); !keys_1_1.done; keys_1_1 = keys_1.next()) {
            var key = keys_1_1.value;
            newBaggage._entries.delete(key);
          }
        } catch (e_1_1) {
          e_1 = { error: e_1_1 };
        } finally {
          try {
            if (keys_1_1 && !keys_1_1.done && (_a2 = keys_1.return))
              _a2.call(keys_1);
          } finally {
            if (e_1)
              throw e_1.error;
          }
        }
        return newBaggage;
      };
      BaggageImpl2.prototype.clear = function() {
        return new BaggageImpl2();
      };
      return BaggageImpl2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/baggage/internal/symbol.js
  var baggageEntryMetadataSymbol = Symbol("BaggageEntryMetadata");

  // node_modules/@opentelemetry/api/build/esm/baggage/utils.js
  var diag = DiagAPI.instance();
  function createBaggage(entries) {
    if (entries === void 0) {
      entries = {};
    }
    return new BaggageImpl(new Map(Object.entries(entries)));
  }
  function baggageEntryMetadataFromString(str) {
    if (typeof str !== "string") {
      diag.error("Cannot create baggage metadata from unknown type: " + typeof str);
      str = "";
    }
    return {
      __TYPE__: baggageEntryMetadataSymbol,
      toString: function() {
        return str;
      }
    };
  }

  // node_modules/@opentelemetry/api/build/esm/context/context.js
  function createContextKey(description) {
    return Symbol.for(description);
  }
  var BaseContext = (
    /** @class */
    function() {
      function BaseContext2(parentContext) {
        var self2 = this;
        self2._currentContext = parentContext ? new Map(parentContext) : /* @__PURE__ */ new Map();
        self2.getValue = function(key) {
          return self2._currentContext.get(key);
        };
        self2.setValue = function(key, value) {
          var context2 = new BaseContext2(self2._currentContext);
          context2._currentContext.set(key, value);
          return context2;
        };
        self2.deleteValue = function(key) {
          var context2 = new BaseContext2(self2._currentContext);
          context2._currentContext.delete(key);
          return context2;
        };
      }
      return BaseContext2;
    }()
  );
  var ROOT_CONTEXT = new BaseContext();

  // node_modules/@opentelemetry/api/build/esm/metrics/NoopMeter.js
  var __extends = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var NoopMeter = (
    /** @class */
    function() {
      function NoopMeter2() {
      }
      NoopMeter2.prototype.createHistogram = function(_name, _options) {
        return NOOP_HISTOGRAM_METRIC;
      };
      NoopMeter2.prototype.createCounter = function(_name, _options) {
        return NOOP_COUNTER_METRIC;
      };
      NoopMeter2.prototype.createUpDownCounter = function(_name, _options) {
        return NOOP_UP_DOWN_COUNTER_METRIC;
      };
      NoopMeter2.prototype.createObservableGauge = function(_name, _options) {
        return NOOP_OBSERVABLE_GAUGE_METRIC;
      };
      NoopMeter2.prototype.createObservableCounter = function(_name, _options) {
        return NOOP_OBSERVABLE_COUNTER_METRIC;
      };
      NoopMeter2.prototype.createObservableUpDownCounter = function(_name, _options) {
        return NOOP_OBSERVABLE_UP_DOWN_COUNTER_METRIC;
      };
      NoopMeter2.prototype.addBatchObservableCallback = function(_callback, _observables) {
      };
      NoopMeter2.prototype.removeBatchObservableCallback = function(_callback) {
      };
      return NoopMeter2;
    }()
  );
  var NoopMetric = (
    /** @class */
    function() {
      function NoopMetric2() {
      }
      return NoopMetric2;
    }()
  );
  var NoopCounterMetric = (
    /** @class */
    function(_super) {
      __extends(NoopCounterMetric2, _super);
      function NoopCounterMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      NoopCounterMetric2.prototype.add = function(_value, _attributes) {
      };
      return NoopCounterMetric2;
    }(NoopMetric)
  );
  var NoopUpDownCounterMetric = (
    /** @class */
    function(_super) {
      __extends(NoopUpDownCounterMetric2, _super);
      function NoopUpDownCounterMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      NoopUpDownCounterMetric2.prototype.add = function(_value, _attributes) {
      };
      return NoopUpDownCounterMetric2;
    }(NoopMetric)
  );
  var NoopHistogramMetric = (
    /** @class */
    function(_super) {
      __extends(NoopHistogramMetric2, _super);
      function NoopHistogramMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      NoopHistogramMetric2.prototype.record = function(_value, _attributes) {
      };
      return NoopHistogramMetric2;
    }(NoopMetric)
  );
  var NoopObservableMetric = (
    /** @class */
    function() {
      function NoopObservableMetric2() {
      }
      NoopObservableMetric2.prototype.addCallback = function(_callback) {
      };
      NoopObservableMetric2.prototype.removeCallback = function(_callback) {
      };
      return NoopObservableMetric2;
    }()
  );
  var NoopObservableCounterMetric = (
    /** @class */
    function(_super) {
      __extends(NoopObservableCounterMetric2, _super);
      function NoopObservableCounterMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      return NoopObservableCounterMetric2;
    }(NoopObservableMetric)
  );
  var NoopObservableGaugeMetric = (
    /** @class */
    function(_super) {
      __extends(NoopObservableGaugeMetric2, _super);
      function NoopObservableGaugeMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      return NoopObservableGaugeMetric2;
    }(NoopObservableMetric)
  );
  var NoopObservableUpDownCounterMetric = (
    /** @class */
    function(_super) {
      __extends(NoopObservableUpDownCounterMetric2, _super);
      function NoopObservableUpDownCounterMetric2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      return NoopObservableUpDownCounterMetric2;
    }(NoopObservableMetric)
  );
  var NOOP_METER = new NoopMeter();
  var NOOP_COUNTER_METRIC = new NoopCounterMetric();
  var NOOP_HISTOGRAM_METRIC = new NoopHistogramMetric();
  var NOOP_UP_DOWN_COUNTER_METRIC = new NoopUpDownCounterMetric();
  var NOOP_OBSERVABLE_COUNTER_METRIC = new NoopObservableCounterMetric();
  var NOOP_OBSERVABLE_GAUGE_METRIC = new NoopObservableGaugeMetric();
  var NOOP_OBSERVABLE_UP_DOWN_COUNTER_METRIC = new NoopObservableUpDownCounterMetric();

  // node_modules/@opentelemetry/api/build/esm/propagation/TextMapPropagator.js
  var defaultTextMapGetter = {
    get: function(carrier, key) {
      if (carrier == null) {
        return void 0;
      }
      return carrier[key];
    },
    keys: function(carrier) {
      if (carrier == null) {
        return [];
      }
      return Object.keys(carrier);
    }
  };
  var defaultTextMapSetter = {
    set: function(carrier, key, value) {
      if (carrier == null) {
        return;
      }
      carrier[key] = value;
    }
  };

  // node_modules/@opentelemetry/api/build/esm/context/NoopContextManager.js
  var __read4 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray3 = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var NoopContextManager = (
    /** @class */
    function() {
      function NoopContextManager2() {
      }
      NoopContextManager2.prototype.active = function() {
        return ROOT_CONTEXT;
      };
      NoopContextManager2.prototype.with = function(_context, fn, thisArg) {
        var args = [];
        for (var _i = 3; _i < arguments.length; _i++) {
          args[_i - 3] = arguments[_i];
        }
        return fn.call.apply(fn, __spreadArray3([thisArg], __read4(args), false));
      };
      NoopContextManager2.prototype.bind = function(_context, target) {
        return target;
      };
      NoopContextManager2.prototype.enable = function() {
        return this;
      };
      NoopContextManager2.prototype.disable = function() {
        return this;
      };
      return NoopContextManager2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/api/context.js
  var __read5 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray4 = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var API_NAME2 = "context";
  var NOOP_CONTEXT_MANAGER = new NoopContextManager();
  var ContextAPI = (
    /** @class */
    function() {
      function ContextAPI2() {
      }
      ContextAPI2.getInstance = function() {
        if (!this._instance) {
          this._instance = new ContextAPI2();
        }
        return this._instance;
      };
      ContextAPI2.prototype.setGlobalContextManager = function(contextManager) {
        return registerGlobal(API_NAME2, contextManager, DiagAPI.instance());
      };
      ContextAPI2.prototype.active = function() {
        return this._getContextManager().active();
      };
      ContextAPI2.prototype.with = function(context2, fn, thisArg) {
        var _a2;
        var args = [];
        for (var _i = 3; _i < arguments.length; _i++) {
          args[_i - 3] = arguments[_i];
        }
        return (_a2 = this._getContextManager()).with.apply(_a2, __spreadArray4([context2, fn, thisArg], __read5(args), false));
      };
      ContextAPI2.prototype.bind = function(context2, target) {
        return this._getContextManager().bind(context2, target);
      };
      ContextAPI2.prototype._getContextManager = function() {
        return getGlobal(API_NAME2) || NOOP_CONTEXT_MANAGER;
      };
      ContextAPI2.prototype.disable = function() {
        this._getContextManager().disable();
        unregisterGlobal(API_NAME2, DiagAPI.instance());
      };
      return ContextAPI2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace/trace_flags.js
  var TraceFlags;
  (function(TraceFlags2) {
    TraceFlags2[TraceFlags2["NONE"] = 0] = "NONE";
    TraceFlags2[TraceFlags2["SAMPLED"] = 1] = "SAMPLED";
  })(TraceFlags || (TraceFlags = {}));

  // node_modules/@opentelemetry/api/build/esm/trace/invalid-span-constants.js
  var INVALID_SPANID = "0000000000000000";
  var INVALID_TRACEID = "00000000000000000000000000000000";
  var INVALID_SPAN_CONTEXT = {
    traceId: INVALID_TRACEID,
    spanId: INVALID_SPANID,
    traceFlags: TraceFlags.NONE
  };

  // node_modules/@opentelemetry/api/build/esm/trace/NonRecordingSpan.js
  var NonRecordingSpan = (
    /** @class */
    function() {
      function NonRecordingSpan2(_spanContext) {
        if (_spanContext === void 0) {
          _spanContext = INVALID_SPAN_CONTEXT;
        }
        this._spanContext = _spanContext;
      }
      NonRecordingSpan2.prototype.spanContext = function() {
        return this._spanContext;
      };
      NonRecordingSpan2.prototype.setAttribute = function(_key, _value) {
        return this;
      };
      NonRecordingSpan2.prototype.setAttributes = function(_attributes) {
        return this;
      };
      NonRecordingSpan2.prototype.addEvent = function(_name, _attributes) {
        return this;
      };
      NonRecordingSpan2.prototype.setStatus = function(_status) {
        return this;
      };
      NonRecordingSpan2.prototype.updateName = function(_name) {
        return this;
      };
      NonRecordingSpan2.prototype.end = function(_endTime) {
      };
      NonRecordingSpan2.prototype.isRecording = function() {
        return false;
      };
      NonRecordingSpan2.prototype.recordException = function(_exception, _time) {
      };
      return NonRecordingSpan2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace/context-utils.js
  var SPAN_KEY = createContextKey("OpenTelemetry Context Key SPAN");
  function getSpan(context2) {
    return context2.getValue(SPAN_KEY) || void 0;
  }
  function getActiveSpan() {
    return getSpan(ContextAPI.getInstance().active());
  }
  function setSpan(context2, span) {
    return context2.setValue(SPAN_KEY, span);
  }
  function deleteSpan(context2) {
    return context2.deleteValue(SPAN_KEY);
  }
  function setSpanContext(context2, spanContext) {
    return setSpan(context2, new NonRecordingSpan(spanContext));
  }
  function getSpanContext(context2) {
    var _a2;
    return (_a2 = getSpan(context2)) === null || _a2 === void 0 ? void 0 : _a2.spanContext();
  }

  // node_modules/@opentelemetry/api/build/esm/trace/spancontext-utils.js
  var VALID_TRACEID_REGEX = /^([0-9a-f]{32})$/i;
  var VALID_SPANID_REGEX = /^[0-9a-f]{16}$/i;
  function isValidTraceId(traceId) {
    return VALID_TRACEID_REGEX.test(traceId) && traceId !== INVALID_TRACEID;
  }
  function isValidSpanId(spanId) {
    return VALID_SPANID_REGEX.test(spanId) && spanId !== INVALID_SPANID;
  }
  function isSpanContextValid(spanContext) {
    return isValidTraceId(spanContext.traceId) && isValidSpanId(spanContext.spanId);
  }
  function wrapSpanContext(spanContext) {
    return new NonRecordingSpan(spanContext);
  }

  // node_modules/@opentelemetry/api/build/esm/trace/NoopTracer.js
  var contextApi = ContextAPI.getInstance();
  var NoopTracer = (
    /** @class */
    function() {
      function NoopTracer2() {
      }
      NoopTracer2.prototype.startSpan = function(name, options, context2) {
        if (context2 === void 0) {
          context2 = contextApi.active();
        }
        var root = Boolean(options === null || options === void 0 ? void 0 : options.root);
        if (root) {
          return new NonRecordingSpan();
        }
        var parentFromContext = context2 && getSpanContext(context2);
        if (isSpanContext(parentFromContext) && isSpanContextValid(parentFromContext)) {
          return new NonRecordingSpan(parentFromContext);
        } else {
          return new NonRecordingSpan();
        }
      };
      NoopTracer2.prototype.startActiveSpan = function(name, arg2, arg3, arg4) {
        var opts;
        var ctx;
        var fn;
        if (arguments.length < 2) {
          return;
        } else if (arguments.length === 2) {
          fn = arg2;
        } else if (arguments.length === 3) {
          opts = arg2;
          fn = arg3;
        } else {
          opts = arg2;
          ctx = arg3;
          fn = arg4;
        }
        var parentContext = ctx !== null && ctx !== void 0 ? ctx : contextApi.active();
        var span = this.startSpan(name, opts, parentContext);
        var contextWithSpanSet = setSpan(parentContext, span);
        return contextApi.with(contextWithSpanSet, fn, void 0, span);
      };
      return NoopTracer2;
    }()
  );
  function isSpanContext(spanContext) {
    return typeof spanContext === "object" && typeof spanContext["spanId"] === "string" && typeof spanContext["traceId"] === "string" && typeof spanContext["traceFlags"] === "number";
  }

  // node_modules/@opentelemetry/api/build/esm/trace/ProxyTracer.js
  var NOOP_TRACER = new NoopTracer();
  var ProxyTracer = (
    /** @class */
    function() {
      function ProxyTracer2(_provider, name, version, options) {
        this._provider = _provider;
        this.name = name;
        this.version = version;
        this.options = options;
      }
      ProxyTracer2.prototype.startSpan = function(name, options, context2) {
        return this._getTracer().startSpan(name, options, context2);
      };
      ProxyTracer2.prototype.startActiveSpan = function(_name, _options, _context, _fn) {
        var tracer = this._getTracer();
        return Reflect.apply(tracer.startActiveSpan, tracer, arguments);
      };
      ProxyTracer2.prototype._getTracer = function() {
        if (this._delegate) {
          return this._delegate;
        }
        var tracer = this._provider.getDelegateTracer(this.name, this.version, this.options);
        if (!tracer) {
          return NOOP_TRACER;
        }
        this._delegate = tracer;
        return this._delegate;
      };
      return ProxyTracer2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace/NoopTracerProvider.js
  var NoopTracerProvider = (
    /** @class */
    function() {
      function NoopTracerProvider2() {
      }
      NoopTracerProvider2.prototype.getTracer = function(_name, _version, _options) {
        return new NoopTracer();
      };
      return NoopTracerProvider2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace/ProxyTracerProvider.js
  var NOOP_TRACER_PROVIDER = new NoopTracerProvider();
  var ProxyTracerProvider = (
    /** @class */
    function() {
      function ProxyTracerProvider2() {
      }
      ProxyTracerProvider2.prototype.getTracer = function(name, version, options) {
        var _a2;
        return (_a2 = this.getDelegateTracer(name, version, options)) !== null && _a2 !== void 0 ? _a2 : new ProxyTracer(this, name, version, options);
      };
      ProxyTracerProvider2.prototype.getDelegate = function() {
        var _a2;
        return (_a2 = this._delegate) !== null && _a2 !== void 0 ? _a2 : NOOP_TRACER_PROVIDER;
      };
      ProxyTracerProvider2.prototype.setDelegate = function(delegate) {
        this._delegate = delegate;
      };
      ProxyTracerProvider2.prototype.getDelegateTracer = function(name, version, options) {
        var _a2;
        return (_a2 = this._delegate) === null || _a2 === void 0 ? void 0 : _a2.getTracer(name, version, options);
      };
      return ProxyTracerProvider2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace/SamplingResult.js
  var SamplingDecision;
  (function(SamplingDecision3) {
    SamplingDecision3[SamplingDecision3["NOT_RECORD"] = 0] = "NOT_RECORD";
    SamplingDecision3[SamplingDecision3["RECORD"] = 1] = "RECORD";
    SamplingDecision3[SamplingDecision3["RECORD_AND_SAMPLED"] = 2] = "RECORD_AND_SAMPLED";
  })(SamplingDecision || (SamplingDecision = {}));

  // node_modules/@opentelemetry/api/build/esm/trace/span_kind.js
  var SpanKind;
  (function(SpanKind2) {
    SpanKind2[SpanKind2["INTERNAL"] = 0] = "INTERNAL";
    SpanKind2[SpanKind2["SERVER"] = 1] = "SERVER";
    SpanKind2[SpanKind2["CLIENT"] = 2] = "CLIENT";
    SpanKind2[SpanKind2["PRODUCER"] = 3] = "PRODUCER";
    SpanKind2[SpanKind2["CONSUMER"] = 4] = "CONSUMER";
  })(SpanKind || (SpanKind = {}));

  // node_modules/@opentelemetry/api/build/esm/trace/status.js
  var SpanStatusCode;
  (function(SpanStatusCode2) {
    SpanStatusCode2[SpanStatusCode2["UNSET"] = 0] = "UNSET";
    SpanStatusCode2[SpanStatusCode2["OK"] = 1] = "OK";
    SpanStatusCode2[SpanStatusCode2["ERROR"] = 2] = "ERROR";
  })(SpanStatusCode || (SpanStatusCode = {}));

  // node_modules/@opentelemetry/api/build/esm/context-api.js
  var context = ContextAPI.getInstance();

  // node_modules/@opentelemetry/api/build/esm/diag-api.js
  var diag2 = DiagAPI.instance();

  // node_modules/@opentelemetry/api/build/esm/metrics/NoopMeterProvider.js
  var NoopMeterProvider = (
    /** @class */
    function() {
      function NoopMeterProvider2() {
      }
      NoopMeterProvider2.prototype.getMeter = function(_name, _version, _options) {
        return NOOP_METER;
      };
      return NoopMeterProvider2;
    }()
  );
  var NOOP_METER_PROVIDER = new NoopMeterProvider();

  // node_modules/@opentelemetry/api/build/esm/api/metrics.js
  var API_NAME3 = "metrics";
  var MetricsAPI = (
    /** @class */
    function() {
      function MetricsAPI2() {
      }
      MetricsAPI2.getInstance = function() {
        if (!this._instance) {
          this._instance = new MetricsAPI2();
        }
        return this._instance;
      };
      MetricsAPI2.prototype.setGlobalMeterProvider = function(provider) {
        return registerGlobal(API_NAME3, provider, DiagAPI.instance());
      };
      MetricsAPI2.prototype.getMeterProvider = function() {
        return getGlobal(API_NAME3) || NOOP_METER_PROVIDER;
      };
      MetricsAPI2.prototype.getMeter = function(name, version, options) {
        return this.getMeterProvider().getMeter(name, version, options);
      };
      MetricsAPI2.prototype.disable = function() {
        unregisterGlobal(API_NAME3, DiagAPI.instance());
      };
      return MetricsAPI2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/metrics-api.js
  var metrics = MetricsAPI.getInstance();

  // node_modules/@opentelemetry/api/build/esm/propagation/NoopTextMapPropagator.js
  var NoopTextMapPropagator = (
    /** @class */
    function() {
      function NoopTextMapPropagator2() {
      }
      NoopTextMapPropagator2.prototype.inject = function(_context, _carrier) {
      };
      NoopTextMapPropagator2.prototype.extract = function(context2, _carrier) {
        return context2;
      };
      NoopTextMapPropagator2.prototype.fields = function() {
        return [];
      };
      return NoopTextMapPropagator2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/baggage/context-helpers.js
  var BAGGAGE_KEY = createContextKey("OpenTelemetry Baggage Key");
  function getBaggage(context2) {
    return context2.getValue(BAGGAGE_KEY) || void 0;
  }
  function getActiveBaggage() {
    return getBaggage(ContextAPI.getInstance().active());
  }
  function setBaggage(context2, baggage) {
    return context2.setValue(BAGGAGE_KEY, baggage);
  }
  function deleteBaggage(context2) {
    return context2.deleteValue(BAGGAGE_KEY);
  }

  // node_modules/@opentelemetry/api/build/esm/api/propagation.js
  var API_NAME4 = "propagation";
  var NOOP_TEXT_MAP_PROPAGATOR = new NoopTextMapPropagator();
  var PropagationAPI = (
    /** @class */
    function() {
      function PropagationAPI2() {
        this.createBaggage = createBaggage;
        this.getBaggage = getBaggage;
        this.getActiveBaggage = getActiveBaggage;
        this.setBaggage = setBaggage;
        this.deleteBaggage = deleteBaggage;
      }
      PropagationAPI2.getInstance = function() {
        if (!this._instance) {
          this._instance = new PropagationAPI2();
        }
        return this._instance;
      };
      PropagationAPI2.prototype.setGlobalPropagator = function(propagator) {
        return registerGlobal(API_NAME4, propagator, DiagAPI.instance());
      };
      PropagationAPI2.prototype.inject = function(context2, carrier, setter) {
        if (setter === void 0) {
          setter = defaultTextMapSetter;
        }
        return this._getGlobalPropagator().inject(context2, carrier, setter);
      };
      PropagationAPI2.prototype.extract = function(context2, carrier, getter) {
        if (getter === void 0) {
          getter = defaultTextMapGetter;
        }
        return this._getGlobalPropagator().extract(context2, carrier, getter);
      };
      PropagationAPI2.prototype.fields = function() {
        return this._getGlobalPropagator().fields();
      };
      PropagationAPI2.prototype.disable = function() {
        unregisterGlobal(API_NAME4, DiagAPI.instance());
      };
      PropagationAPI2.prototype._getGlobalPropagator = function() {
        return getGlobal(API_NAME4) || NOOP_TEXT_MAP_PROPAGATOR;
      };
      return PropagationAPI2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/propagation-api.js
  var propagation = PropagationAPI.getInstance();

  // node_modules/@opentelemetry/api/build/esm/api/trace.js
  var API_NAME5 = "trace";
  var TraceAPI = (
    /** @class */
    function() {
      function TraceAPI2() {
        this._proxyTracerProvider = new ProxyTracerProvider();
        this.wrapSpanContext = wrapSpanContext;
        this.isSpanContextValid = isSpanContextValid;
        this.deleteSpan = deleteSpan;
        this.getSpan = getSpan;
        this.getActiveSpan = getActiveSpan;
        this.getSpanContext = getSpanContext;
        this.setSpan = setSpan;
        this.setSpanContext = setSpanContext;
      }
      TraceAPI2.getInstance = function() {
        if (!this._instance) {
          this._instance = new TraceAPI2();
        }
        return this._instance;
      };
      TraceAPI2.prototype.setGlobalTracerProvider = function(provider) {
        var success = registerGlobal(API_NAME5, this._proxyTracerProvider, DiagAPI.instance());
        if (success) {
          this._proxyTracerProvider.setDelegate(provider);
        }
        return success;
      };
      TraceAPI2.prototype.getTracerProvider = function() {
        return getGlobal(API_NAME5) || this._proxyTracerProvider;
      };
      TraceAPI2.prototype.getTracer = function(name, version) {
        return this.getTracerProvider().getTracer(name, version);
      };
      TraceAPI2.prototype.disable = function() {
        unregisterGlobal(API_NAME5, DiagAPI.instance());
        this._proxyTracerProvider = new ProxyTracerProvider();
      };
      return TraceAPI2;
    }()
  );

  // node_modules/@opentelemetry/api/build/esm/trace-api.js
  var trace = TraceAPI.getInstance();

  // node_modules/@opentelemetry/core/build/esm/trace/suppress-tracing.js
  var SUPPRESS_TRACING_KEY = createContextKey("OpenTelemetry SDK Context Key SUPPRESS_TRACING");
  function suppressTracing(context2) {
    return context2.setValue(SUPPRESS_TRACING_KEY, true);
  }
  function isTracingSuppressed(context2) {
    return context2.getValue(SUPPRESS_TRACING_KEY) === true;
  }

  // node_modules/@opentelemetry/core/build/esm/baggage/constants.js
  var BAGGAGE_KEY_PAIR_SEPARATOR = "=";
  var BAGGAGE_PROPERTIES_SEPARATOR = ";";
  var BAGGAGE_ITEMS_SEPARATOR = ",";
  var BAGGAGE_HEADER = "baggage";
  var BAGGAGE_MAX_NAME_VALUE_PAIRS = 180;
  var BAGGAGE_MAX_PER_NAME_VALUE_PAIRS = 4096;
  var BAGGAGE_MAX_TOTAL_LENGTH = 8192;

  // node_modules/@opentelemetry/core/build/esm/baggage/utils.js
  var utils_exports = {};
  __export(utils_exports, {
    getKeyPairs: () => getKeyPairs,
    parseKeyPairsIntoRecord: () => parseKeyPairsIntoRecord,
    parsePairKeyValue: () => parsePairKeyValue,
    serializeKeyPairs: () => serializeKeyPairs
  });
  var __read6 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  function serializeKeyPairs(keyPairs) {
    return keyPairs.reduce(function(hValue, current) {
      var value = "" + hValue + (hValue !== "" ? BAGGAGE_ITEMS_SEPARATOR : "") + current;
      return value.length > BAGGAGE_MAX_TOTAL_LENGTH ? hValue : value;
    }, "");
  }
  function getKeyPairs(baggage) {
    return baggage.getAllEntries().map(function(_a2) {
      var _b = __read6(_a2, 2), key = _b[0], value = _b[1];
      var entry = encodeURIComponent(key) + "=" + encodeURIComponent(value.value);
      if (value.metadata !== void 0) {
        entry += BAGGAGE_PROPERTIES_SEPARATOR + value.metadata.toString();
      }
      return entry;
    });
  }
  function parsePairKeyValue(entry) {
    var valueProps = entry.split(BAGGAGE_PROPERTIES_SEPARATOR);
    if (valueProps.length <= 0)
      return;
    var keyPairPart = valueProps.shift();
    if (!keyPairPart)
      return;
    var keyPair = keyPairPart.split(BAGGAGE_KEY_PAIR_SEPARATOR);
    if (keyPair.length !== 2)
      return;
    var key = decodeURIComponent(keyPair[0].trim());
    var value = decodeURIComponent(keyPair[1].trim());
    var metadata;
    if (valueProps.length > 0) {
      metadata = baggageEntryMetadataFromString(valueProps.join(BAGGAGE_PROPERTIES_SEPARATOR));
    }
    return { key, value, metadata };
  }
  function parseKeyPairsIntoRecord(value) {
    if (typeof value !== "string" || value.length === 0)
      return {};
    return value.split(BAGGAGE_ITEMS_SEPARATOR).map(function(entry) {
      return parsePairKeyValue(entry);
    }).filter(function(keyPair) {
      return keyPair !== void 0 && keyPair.value.length > 0;
    }).reduce(function(headers, keyPair) {
      headers[keyPair.key] = keyPair.value;
      return headers;
    }, {});
  }

  // node_modules/@opentelemetry/core/build/esm/baggage/propagation/W3CBaggagePropagator.js
  var W3CBaggagePropagator = (
    /** @class */
    function() {
      function W3CBaggagePropagator2() {
      }
      W3CBaggagePropagator2.prototype.inject = function(context2, carrier, setter) {
        var baggage = propagation.getBaggage(context2);
        if (!baggage || isTracingSuppressed(context2))
          return;
        var keyPairs = getKeyPairs(baggage).filter(function(pair) {
          return pair.length <= BAGGAGE_MAX_PER_NAME_VALUE_PAIRS;
        }).slice(0, BAGGAGE_MAX_NAME_VALUE_PAIRS);
        var headerValue = serializeKeyPairs(keyPairs);
        if (headerValue.length > 0) {
          setter.set(carrier, BAGGAGE_HEADER, headerValue);
        }
      };
      W3CBaggagePropagator2.prototype.extract = function(context2, carrier, getter) {
        var headerValue = getter.get(carrier, BAGGAGE_HEADER);
        var baggageString = Array.isArray(headerValue) ? headerValue.join(BAGGAGE_ITEMS_SEPARATOR) : headerValue;
        if (!baggageString)
          return context2;
        var baggage = {};
        if (baggageString.length === 0) {
          return context2;
        }
        var pairs = baggageString.split(BAGGAGE_ITEMS_SEPARATOR);
        pairs.forEach(function(entry) {
          var keyPair = parsePairKeyValue(entry);
          if (keyPair) {
            var baggageEntry = { value: keyPair.value };
            if (keyPair.metadata) {
              baggageEntry.metadata = keyPair.metadata;
            }
            baggage[keyPair.key] = baggageEntry;
          }
        });
        if (Object.entries(baggage).length === 0) {
          return context2;
        }
        return propagation.setBaggage(context2, propagation.createBaggage(baggage));
      };
      W3CBaggagePropagator2.prototype.fields = function() {
        return [BAGGAGE_HEADER];
      };
      return W3CBaggagePropagator2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/common/attributes.js
  var __values2 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var __read7 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  function sanitizeAttributes(attributes) {
    var e_1, _a2;
    var out = {};
    if (typeof attributes !== "object" || attributes == null) {
      return out;
    }
    try {
      for (var _b = __values2(Object.entries(attributes)), _c = _b.next(); !_c.done; _c = _b.next()) {
        var _d = __read7(_c.value, 2), key = _d[0], val = _d[1];
        if (!isAttributeKey(key)) {
          diag2.warn("Invalid attribute key: " + key);
          continue;
        }
        if (!isAttributeValue(val)) {
          diag2.warn("Invalid attribute value set for key: " + key);
          continue;
        }
        if (Array.isArray(val)) {
          out[key] = val.slice();
        } else {
          out[key] = val;
        }
      }
    } catch (e_1_1) {
      e_1 = { error: e_1_1 };
    } finally {
      try {
        if (_c && !_c.done && (_a2 = _b.return))
          _a2.call(_b);
      } finally {
        if (e_1)
          throw e_1.error;
      }
    }
    return out;
  }
  function isAttributeKey(key) {
    return typeof key === "string" && key.length > 0;
  }
  function isAttributeValue(val) {
    if (val == null) {
      return true;
    }
    if (Array.isArray(val)) {
      return isHomogeneousAttributeValueArray(val);
    }
    return isValidPrimitiveAttributeValue(val);
  }
  function isHomogeneousAttributeValueArray(arr) {
    var e_2, _a2;
    var type;
    try {
      for (var arr_1 = __values2(arr), arr_1_1 = arr_1.next(); !arr_1_1.done; arr_1_1 = arr_1.next()) {
        var element = arr_1_1.value;
        if (element == null)
          continue;
        if (!type) {
          if (isValidPrimitiveAttributeValue(element)) {
            type = typeof element;
            continue;
          }
          return false;
        }
        if (typeof element === type) {
          continue;
        }
        return false;
      }
    } catch (e_2_1) {
      e_2 = { error: e_2_1 };
    } finally {
      try {
        if (arr_1_1 && !arr_1_1.done && (_a2 = arr_1.return))
          _a2.call(arr_1);
      } finally {
        if (e_2)
          throw e_2.error;
      }
    }
    return true;
  }
  function isValidPrimitiveAttributeValue(val) {
    switch (typeof val) {
      case "number":
      case "boolean":
      case "string":
        return true;
    }
    return false;
  }

  // node_modules/@opentelemetry/core/build/esm/common/logging-error-handler.js
  function loggingErrorHandler() {
    return function(ex) {
      diag2.error(stringifyException(ex));
    };
  }
  function stringifyException(ex) {
    if (typeof ex === "string") {
      return ex;
    } else {
      return JSON.stringify(flattenException(ex));
    }
  }
  function flattenException(ex) {
    var result = {};
    var current = ex;
    while (current !== null) {
      Object.getOwnPropertyNames(current).forEach(function(propertyName) {
        if (result[propertyName])
          return;
        var value = current[propertyName];
        if (value) {
          result[propertyName] = String(value);
        }
      });
      current = Object.getPrototypeOf(current);
    }
    return result;
  }

  // node_modules/@opentelemetry/core/build/esm/common/global-error-handler.js
  var delegateHandler = loggingErrorHandler();
  function globalErrorHandler(ex) {
    try {
      delegateHandler(ex);
    } catch (_a2) {
    }
  }

  // node_modules/@opentelemetry/core/build/esm/utils/sampling.js
  var TracesSamplerValues;
  (function(TracesSamplerValues2) {
    TracesSamplerValues2["AlwaysOff"] = "always_off";
    TracesSamplerValues2["AlwaysOn"] = "always_on";
    TracesSamplerValues2["ParentBasedAlwaysOff"] = "parentbased_always_off";
    TracesSamplerValues2["ParentBasedAlwaysOn"] = "parentbased_always_on";
    TracesSamplerValues2["ParentBasedTraceIdRatio"] = "parentbased_traceidratio";
    TracesSamplerValues2["TraceIdRatio"] = "traceidratio";
  })(TracesSamplerValues || (TracesSamplerValues = {}));

  // node_modules/@opentelemetry/core/build/esm/platform/browser/globalThis.js
  var _globalThis2 = typeof globalThis === "object" ? globalThis : typeof self === "object" ? self : typeof window === "object" ? window : typeof global === "object" ? global : {};

  // node_modules/@opentelemetry/core/build/esm/utils/environment.js
  var DEFAULT_LIST_SEPARATOR = ",";
  var ENVIRONMENT_BOOLEAN_KEYS = ["OTEL_SDK_DISABLED"];
  function isEnvVarABoolean(key) {
    return ENVIRONMENT_BOOLEAN_KEYS.indexOf(key) > -1;
  }
  var ENVIRONMENT_NUMBERS_KEYS = [
    "OTEL_BSP_EXPORT_TIMEOUT",
    "OTEL_BSP_MAX_EXPORT_BATCH_SIZE",
    "OTEL_BSP_MAX_QUEUE_SIZE",
    "OTEL_BSP_SCHEDULE_DELAY",
    "OTEL_BLRP_EXPORT_TIMEOUT",
    "OTEL_BLRP_MAX_EXPORT_BATCH_SIZE",
    "OTEL_BLRP_MAX_QUEUE_SIZE",
    "OTEL_BLRP_SCHEDULE_DELAY",
    "OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT",
    "OTEL_ATTRIBUTE_COUNT_LIMIT",
    "OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT",
    "OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT",
    "OTEL_LOGRECORD_ATTRIBUTE_VALUE_LENGTH_LIMIT",
    "OTEL_LOGRECORD_ATTRIBUTE_COUNT_LIMIT",
    "OTEL_SPAN_EVENT_COUNT_LIMIT",
    "OTEL_SPAN_LINK_COUNT_LIMIT",
    "OTEL_SPAN_ATTRIBUTE_PER_EVENT_COUNT_LIMIT",
    "OTEL_SPAN_ATTRIBUTE_PER_LINK_COUNT_LIMIT",
    "OTEL_EXPORTER_OTLP_TIMEOUT",
    "OTEL_EXPORTER_OTLP_TRACES_TIMEOUT",
    "OTEL_EXPORTER_OTLP_METRICS_TIMEOUT",
    "OTEL_EXPORTER_OTLP_LOGS_TIMEOUT",
    "OTEL_EXPORTER_JAEGER_AGENT_PORT"
  ];
  function isEnvVarANumber(key) {
    return ENVIRONMENT_NUMBERS_KEYS.indexOf(key) > -1;
  }
  var ENVIRONMENT_LISTS_KEYS = [
    "OTEL_NO_PATCH_MODULES",
    "OTEL_PROPAGATORS"
  ];
  function isEnvVarAList(key) {
    return ENVIRONMENT_LISTS_KEYS.indexOf(key) > -1;
  }
  var DEFAULT_ATTRIBUTE_VALUE_LENGTH_LIMIT = Infinity;
  var DEFAULT_ATTRIBUTE_COUNT_LIMIT = 128;
  var DEFAULT_SPAN_ATTRIBUTE_PER_EVENT_COUNT_LIMIT = 128;
  var DEFAULT_SPAN_ATTRIBUTE_PER_LINK_COUNT_LIMIT = 128;
  var DEFAULT_ENVIRONMENT = {
    OTEL_SDK_DISABLED: false,
    CONTAINER_NAME: "",
    ECS_CONTAINER_METADATA_URI_V4: "",
    ECS_CONTAINER_METADATA_URI: "",
    HOSTNAME: "",
    KUBERNETES_SERVICE_HOST: "",
    NAMESPACE: "",
    OTEL_BSP_EXPORT_TIMEOUT: 3e4,
    OTEL_BSP_MAX_EXPORT_BATCH_SIZE: 512,
    OTEL_BSP_MAX_QUEUE_SIZE: 2048,
    OTEL_BSP_SCHEDULE_DELAY: 5e3,
    OTEL_BLRP_EXPORT_TIMEOUT: 3e4,
    OTEL_BLRP_MAX_EXPORT_BATCH_SIZE: 512,
    OTEL_BLRP_MAX_QUEUE_SIZE: 2048,
    OTEL_BLRP_SCHEDULE_DELAY: 5e3,
    OTEL_EXPORTER_JAEGER_AGENT_HOST: "",
    OTEL_EXPORTER_JAEGER_AGENT_PORT: 6832,
    OTEL_EXPORTER_JAEGER_ENDPOINT: "",
    OTEL_EXPORTER_JAEGER_PASSWORD: "",
    OTEL_EXPORTER_JAEGER_USER: "",
    OTEL_EXPORTER_OTLP_ENDPOINT: "",
    OTEL_EXPORTER_OTLP_TRACES_ENDPOINT: "",
    OTEL_EXPORTER_OTLP_METRICS_ENDPOINT: "",
    OTEL_EXPORTER_OTLP_LOGS_ENDPOINT: "",
    OTEL_EXPORTER_OTLP_HEADERS: "",
    OTEL_EXPORTER_OTLP_TRACES_HEADERS: "",
    OTEL_EXPORTER_OTLP_METRICS_HEADERS: "",
    OTEL_EXPORTER_OTLP_LOGS_HEADERS: "",
    OTEL_EXPORTER_OTLP_TIMEOUT: 1e4,
    OTEL_EXPORTER_OTLP_TRACES_TIMEOUT: 1e4,
    OTEL_EXPORTER_OTLP_METRICS_TIMEOUT: 1e4,
    OTEL_EXPORTER_OTLP_LOGS_TIMEOUT: 1e4,
    OTEL_EXPORTER_ZIPKIN_ENDPOINT: "http://localhost:9411/api/v2/spans",
    OTEL_LOG_LEVEL: DiagLogLevel.INFO,
    OTEL_NO_PATCH_MODULES: [],
    OTEL_PROPAGATORS: ["tracecontext", "baggage"],
    OTEL_RESOURCE_ATTRIBUTES: "",
    OTEL_SERVICE_NAME: "",
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT: DEFAULT_ATTRIBUTE_VALUE_LENGTH_LIMIT,
    OTEL_ATTRIBUTE_COUNT_LIMIT: DEFAULT_ATTRIBUTE_COUNT_LIMIT,
    OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT: DEFAULT_ATTRIBUTE_VALUE_LENGTH_LIMIT,
    OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT: DEFAULT_ATTRIBUTE_COUNT_LIMIT,
    OTEL_LOGRECORD_ATTRIBUTE_VALUE_LENGTH_LIMIT: DEFAULT_ATTRIBUTE_VALUE_LENGTH_LIMIT,
    OTEL_LOGRECORD_ATTRIBUTE_COUNT_LIMIT: DEFAULT_ATTRIBUTE_COUNT_LIMIT,
    OTEL_SPAN_EVENT_COUNT_LIMIT: 128,
    OTEL_SPAN_LINK_COUNT_LIMIT: 128,
    OTEL_SPAN_ATTRIBUTE_PER_EVENT_COUNT_LIMIT: DEFAULT_SPAN_ATTRIBUTE_PER_EVENT_COUNT_LIMIT,
    OTEL_SPAN_ATTRIBUTE_PER_LINK_COUNT_LIMIT: DEFAULT_SPAN_ATTRIBUTE_PER_LINK_COUNT_LIMIT,
    OTEL_TRACES_EXPORTER: "",
    OTEL_TRACES_SAMPLER: TracesSamplerValues.ParentBasedAlwaysOn,
    OTEL_TRACES_SAMPLER_ARG: "",
    OTEL_LOGS_EXPORTER: "",
    OTEL_EXPORTER_OTLP_INSECURE: "",
    OTEL_EXPORTER_OTLP_TRACES_INSECURE: "",
    OTEL_EXPORTER_OTLP_METRICS_INSECURE: "",
    OTEL_EXPORTER_OTLP_LOGS_INSECURE: "",
    OTEL_EXPORTER_OTLP_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_TRACES_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_METRICS_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_LOGS_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_COMPRESSION: "",
    OTEL_EXPORTER_OTLP_TRACES_COMPRESSION: "",
    OTEL_EXPORTER_OTLP_METRICS_COMPRESSION: "",
    OTEL_EXPORTER_OTLP_LOGS_COMPRESSION: "",
    OTEL_EXPORTER_OTLP_CLIENT_KEY: "",
    OTEL_EXPORTER_OTLP_TRACES_CLIENT_KEY: "",
    OTEL_EXPORTER_OTLP_METRICS_CLIENT_KEY: "",
    OTEL_EXPORTER_OTLP_LOGS_CLIENT_KEY: "",
    OTEL_EXPORTER_OTLP_CLIENT_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_TRACES_CLIENT_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_METRICS_CLIENT_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_LOGS_CLIENT_CERTIFICATE: "",
    OTEL_EXPORTER_OTLP_PROTOCOL: "http/protobuf",
    OTEL_EXPORTER_OTLP_TRACES_PROTOCOL: "http/protobuf",
    OTEL_EXPORTER_OTLP_METRICS_PROTOCOL: "http/protobuf",
    OTEL_EXPORTER_OTLP_LOGS_PROTOCOL: "http/protobuf",
    OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE: "cumulative"
  };
  function parseBoolean(key, environment, values) {
    if (typeof values[key] === "undefined") {
      return;
    }
    var value = String(values[key]);
    environment[key] = value.toLowerCase() === "true";
  }
  function parseNumber(name, environment, values, min, max) {
    if (min === void 0) {
      min = -Infinity;
    }
    if (max === void 0) {
      max = Infinity;
    }
    if (typeof values[name] !== "undefined") {
      var value = Number(values[name]);
      if (!isNaN(value)) {
        if (value < min) {
          environment[name] = min;
        } else if (value > max) {
          environment[name] = max;
        } else {
          environment[name] = value;
        }
      }
    }
  }
  function parseStringList(name, output, input, separator) {
    if (separator === void 0) {
      separator = DEFAULT_LIST_SEPARATOR;
    }
    var givenValue = input[name];
    if (typeof givenValue === "string") {
      output[name] = givenValue.split(separator).map(function(v2) {
        return v2.trim();
      });
    }
  }
  var logLevelMap = {
    ALL: DiagLogLevel.ALL,
    VERBOSE: DiagLogLevel.VERBOSE,
    DEBUG: DiagLogLevel.DEBUG,
    INFO: DiagLogLevel.INFO,
    WARN: DiagLogLevel.WARN,
    ERROR: DiagLogLevel.ERROR,
    NONE: DiagLogLevel.NONE
  };
  function setLogLevelFromEnv(key, environment, values) {
    var value = values[key];
    if (typeof value === "string") {
      var theLevel = logLevelMap[value.toUpperCase()];
      if (theLevel != null) {
        environment[key] = theLevel;
      }
    }
  }
  function parseEnvironment(values) {
    var environment = {};
    for (var env2 in DEFAULT_ENVIRONMENT) {
      var key = env2;
      switch (key) {
        case "OTEL_LOG_LEVEL":
          setLogLevelFromEnv(key, environment, values);
          break;
        default:
          if (isEnvVarABoolean(key)) {
            parseBoolean(key, environment, values);
          } else if (isEnvVarANumber(key)) {
            parseNumber(key, environment, values);
          } else if (isEnvVarAList(key)) {
            parseStringList(key, environment, values);
          } else {
            var value = values[key];
            if (typeof value !== "undefined" && value !== null) {
              environment[key] = String(value);
            }
          }
      }
    }
    return environment;
  }
  function getEnvWithoutDefaults() {
    return typeof process !== "undefined" && process && process.env ? parseEnvironment(process.env) : parseEnvironment(_globalThis2);
  }

  // node_modules/@opentelemetry/core/build/esm/platform/browser/environment.js
  function getEnv() {
    var globalEnv = parseEnvironment(_globalThis2);
    return Object.assign({}, DEFAULT_ENVIRONMENT, globalEnv);
  }

  // node_modules/@opentelemetry/core/build/esm/platform/browser/hex-to-base64.js
  function hexToBase64(hexStr) {
    var hexStrLen = hexStr.length;
    var hexAsciiCharsStr = "";
    for (var i2 = 0; i2 < hexStrLen; i2 += 2) {
      var hexPair = hexStr.substring(i2, i2 + 2);
      var hexVal = parseInt(hexPair, 16);
      hexAsciiCharsStr += String.fromCharCode(hexVal);
    }
    return btoa(hexAsciiCharsStr);
  }

  // node_modules/@opentelemetry/core/build/esm/platform/browser/performance.js
  var otperformance = performance;

  // node_modules/@opentelemetry/core/build/esm/version.js
  var VERSION2 = "1.13.0";

  // node_modules/@opentelemetry/semantic-conventions/build/esm/trace/SemanticAttributes.js
  var SemanticAttributes = {
    /**
     * The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable).
     *
     * Note: This may be different from `faas.id` if an alias is involved.
     */
    AWS_LAMBDA_INVOKED_ARN: "aws.lambda.invoked_arn",
    /**
     * An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers.
     */
    DB_SYSTEM: "db.system",
    /**
     * The connection string used to connect to the database. It is recommended to remove embedded credentials.
     */
    DB_CONNECTION_STRING: "db.connection_string",
    /**
     * Username for accessing the database.
     */
    DB_USER: "db.user",
    /**
     * The fully-qualified class name of the [Java Database Connectivity (JDBC)](https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/) driver used to connect.
     */
    DB_JDBC_DRIVER_CLASSNAME: "db.jdbc.driver_classname",
    /**
     * If no [tech-specific attribute](#call-level-attributes-for-specific-technologies) is defined, this attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails).
     *
     * Note: In some SQL databases, the database name to be used is called &#34;schema name&#34;.
     */
    DB_NAME: "db.name",
    /**
     * The database statement being executed.
     *
     * Note: The value may be sanitized to exclude sensitive information.
     */
    DB_STATEMENT: "db.statement",
    /**
     * The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword.
     *
     * Note: When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted.
     */
    DB_OPERATION: "db.operation",
    /**
     * The Microsoft SQL Server [instance name](https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance.
     *
     * Note: If setting a `db.mssql.instance_name`, `net.peer.port` is no longer required (but still recommended if non-standard).
     */
    DB_MSSQL_INSTANCE_NAME: "db.mssql.instance_name",
    /**
     * The name of the keyspace being accessed. To be used instead of the generic `db.name` attribute.
     */
    DB_CASSANDRA_KEYSPACE: "db.cassandra.keyspace",
    /**
     * The fetch size used for paging, i.e. how many rows will be returned at once.
     */
    DB_CASSANDRA_PAGE_SIZE: "db.cassandra.page_size",
    /**
     * The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).
     */
    DB_CASSANDRA_CONSISTENCY_LEVEL: "db.cassandra.consistency_level",
    /**
     * The name of the primary table that the operation is acting upon, including the schema name (if applicable).
     *
     * Note: This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set.
     */
    DB_CASSANDRA_TABLE: "db.cassandra.table",
    /**
     * Whether or not the query is idempotent.
     */
    DB_CASSANDRA_IDEMPOTENCE: "db.cassandra.idempotence",
    /**
     * The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.
     */
    DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT: "db.cassandra.speculative_execution_count",
    /**
     * The ID of the coordinating node for a query.
     */
    DB_CASSANDRA_COORDINATOR_ID: "db.cassandra.coordinator.id",
    /**
     * The data center of the coordinating node for a query.
     */
    DB_CASSANDRA_COORDINATOR_DC: "db.cassandra.coordinator.dc",
    /**
     * The [HBase namespace](https://hbase.apache.org/book.html#_namespace) being accessed. To be used instead of the generic `db.name` attribute.
     */
    DB_HBASE_NAMESPACE: "db.hbase.namespace",
    /**
     * The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute.
     */
    DB_REDIS_DATABASE_INDEX: "db.redis.database_index",
    /**
     * The collection being accessed within the database stated in `db.name`.
     */
    DB_MONGODB_COLLECTION: "db.mongodb.collection",
    /**
     * The name of the primary table that the operation is acting upon, including the schema name (if applicable).
     *
     * Note: It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set.
     */
    DB_SQL_TABLE: "db.sql.table",
    /**
     * The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.
     */
    EXCEPTION_TYPE: "exception.type",
    /**
     * The exception message.
     */
    EXCEPTION_MESSAGE: "exception.message",
    /**
     * A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.
     */
    EXCEPTION_STACKTRACE: "exception.stacktrace",
    /**
      * SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span.
      *
      * Note: An exception is considered to have escaped (or left) the scope of a span,
    if that span is ended while the exception is still logically &#34;in flight&#34;.
    This may be actually &#34;in flight&#34; in some languages (e.g. if the exception
    is passed to a Context manager&#39;s `__exit__` method in Python) but will
    usually be caught at the point of recording the exception in most languages.
    
    It is usually not possible to determine at the point where an exception is thrown
    whether it will escape the scope of a span.
    However, it is trivial to know that an exception
    will escape, if one checks for an active exception just before ending the span,
    as done in the [example above](#exception-end-example).
    
    It follows that an exception may still escape the scope of the span
    even if the `exception.escaped` attribute was not set or set to false,
    since the event might have been recorded at a time where it was not
    clear whether the exception will escape.
      */
    EXCEPTION_ESCAPED: "exception.escaped",
    /**
     * Type of the trigger on which the function is executed.
     */
    FAAS_TRIGGER: "faas.trigger",
    /**
     * The execution ID of the current function execution.
     */
    FAAS_EXECUTION: "faas.execution",
    /**
     * The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name.
     */
    FAAS_DOCUMENT_COLLECTION: "faas.document.collection",
    /**
     * Describes the type of the operation that was performed on the data.
     */
    FAAS_DOCUMENT_OPERATION: "faas.document.operation",
    /**
     * A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
     */
    FAAS_DOCUMENT_TIME: "faas.document.time",
    /**
     * The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name.
     */
    FAAS_DOCUMENT_NAME: "faas.document.name",
    /**
     * A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
     */
    FAAS_TIME: "faas.time",
    /**
     * A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm).
     */
    FAAS_CRON: "faas.cron",
    /**
     * A boolean that is true if the serverless function is executed for the first time (aka cold-start).
     */
    FAAS_COLDSTART: "faas.coldstart",
    /**
     * The name of the invoked function.
     *
     * Note: SHOULD be equal to the `faas.name` resource attribute of the invoked function.
     */
    FAAS_INVOKED_NAME: "faas.invoked_name",
    /**
     * The cloud provider of the invoked function.
     *
     * Note: SHOULD be equal to the `cloud.provider` resource attribute of the invoked function.
     */
    FAAS_INVOKED_PROVIDER: "faas.invoked_provider",
    /**
     * The cloud region of the invoked function.
     *
     * Note: SHOULD be equal to the `cloud.region` resource attribute of the invoked function.
     */
    FAAS_INVOKED_REGION: "faas.invoked_region",
    /**
     * Transport protocol used. See note below.
     */
    NET_TRANSPORT: "net.transport",
    /**
     * Remote address of the peer (dotted decimal for IPv4 or [RFC5952](https://tools.ietf.org/html/rfc5952) for IPv6).
     */
    NET_PEER_IP: "net.peer.ip",
    /**
     * Remote port number.
     */
    NET_PEER_PORT: "net.peer.port",
    /**
     * Remote hostname or similar, see note below.
     */
    NET_PEER_NAME: "net.peer.name",
    /**
     * Like `net.peer.ip` but for the host IP. Useful in case of a multi-IP host.
     */
    NET_HOST_IP: "net.host.ip",
    /**
     * Like `net.peer.port` but for the host port.
     */
    NET_HOST_PORT: "net.host.port",
    /**
     * Local hostname or similar, see note below.
     */
    NET_HOST_NAME: "net.host.name",
    /**
     * The internet connection type currently being used by the host.
     */
    NET_HOST_CONNECTION_TYPE: "net.host.connection.type",
    /**
     * This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.
     */
    NET_HOST_CONNECTION_SUBTYPE: "net.host.connection.subtype",
    /**
     * The name of the mobile carrier.
     */
    NET_HOST_CARRIER_NAME: "net.host.carrier.name",
    /**
     * The mobile carrier country code.
     */
    NET_HOST_CARRIER_MCC: "net.host.carrier.mcc",
    /**
     * The mobile carrier network code.
     */
    NET_HOST_CARRIER_MNC: "net.host.carrier.mnc",
    /**
     * The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
     */
    NET_HOST_CARRIER_ICC: "net.host.carrier.icc",
    /**
     * The [`service.name`](../../resource/semantic_conventions/README.md#service) of the remote service. SHOULD be equal to the actual `service.name` resource attribute of the remote service if any.
     */
    PEER_SERVICE: "peer.service",
    /**
     * Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system.
     */
    ENDUSER_ID: "enduser.id",
    /**
     * Actual/assumed role the client is making the request under extracted from token or application security context.
     */
    ENDUSER_ROLE: "enduser.role",
    /**
     * Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html).
     */
    ENDUSER_SCOPE: "enduser.scope",
    /**
     * Current &#34;managed&#34; thread ID (as opposed to OS thread ID).
     */
    THREAD_ID: "thread.id",
    /**
     * Current thread name.
     */
    THREAD_NAME: "thread.name",
    /**
     * The method or function name, or equivalent (usually rightmost part of the code unit&#39;s name).
     */
    CODE_FUNCTION: "code.function",
    /**
     * The &#34;namespace&#34; within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit.
     */
    CODE_NAMESPACE: "code.namespace",
    /**
     * The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).
     */
    CODE_FILEPATH: "code.filepath",
    /**
     * The line number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`.
     */
    CODE_LINENO: "code.lineno",
    /**
     * HTTP request method.
     */
    HTTP_METHOD: "http.method",
    /**
     * Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless.
     *
     * Note: `http.url` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case the attribute&#39;s value should be `https://www.example.com/`.
     */
    HTTP_URL: "http.url",
    /**
     * The full request target as passed in a HTTP request line or equivalent.
     */
    HTTP_TARGET: "http.target",
    /**
     * The value of the [HTTP host header](https://tools.ietf.org/html/rfc7230#section-5.4). An empty Host header should also be reported, see note.
     *
     * Note: When the header is present but empty the attribute SHOULD be set to the empty string. Note that this is a valid situation that is expected in certain cases, according the aforementioned [section of RFC 7230](https://tools.ietf.org/html/rfc7230#section-5.4). When the header is not set the attribute MUST NOT be set.
     */
    HTTP_HOST: "http.host",
    /**
     * The URI scheme identifying the used protocol.
     */
    HTTP_SCHEME: "http.scheme",
    /**
     * [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).
     */
    HTTP_STATUS_CODE: "http.status_code",
    /**
     * Kind of HTTP protocol used.
     *
     * Note: If `net.transport` is not specified, it can be assumed to be `IP.TCP` except if `http.flavor` is `QUIC`, in which case `IP.UDP` is assumed.
     */
    HTTP_FLAVOR: "http.flavor",
    /**
     * Value of the [HTTP User-Agent](https://tools.ietf.org/html/rfc7231#section-5.5.3) header sent by the client.
     */
    HTTP_USER_AGENT: "http.user_agent",
    /**
     * The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://tools.ietf.org/html/rfc7230#section-3.3.2) header. For requests using transport encoding, this should be the compressed size.
     */
    HTTP_REQUEST_CONTENT_LENGTH: "http.request_content_length",
    /**
     * The size of the uncompressed request payload body after transport decoding. Not set if transport encoding not used.
     */
    HTTP_REQUEST_CONTENT_LENGTH_UNCOMPRESSED: "http.request_content_length_uncompressed",
    /**
     * The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://tools.ietf.org/html/rfc7230#section-3.3.2) header. For requests using transport encoding, this should be the compressed size.
     */
    HTTP_RESPONSE_CONTENT_LENGTH: "http.response_content_length",
    /**
     * The size of the uncompressed response payload body after transport decoding. Not set if transport encoding not used.
     */
    HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED: "http.response_content_length_uncompressed",
    /**
     * The primary server name of the matched virtual host. This should be obtained via configuration. If no such configuration can be obtained, this attribute MUST NOT be set ( `net.host.name` should be used instead).
     *
     * Note: `http.url` is usually not readily available on the server side but would have to be assembled in a cumbersome and sometimes lossy process from other information (see e.g. open-telemetry/opentelemetry-python/pull/148). It is thus preferred to supply the raw data that is available.
     */
    HTTP_SERVER_NAME: "http.server_name",
    /**
     * The matched route (path template).
     */
    HTTP_ROUTE: "http.route",
    /**
      * The IP address of the original client behind all proxies, if known (e.g. from [X-Forwarded-For](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For)).
      *
      * Note: This is not necessarily the same as `net.peer.ip`, which would
    identify the network-level peer, which may be a proxy.
    
    This attribute should be set when a source of information different
    from the one used for `net.peer.ip`, is available even if that other
    source just confirms the same value as `net.peer.ip`.
    Rationale: For `net.peer.ip`, one typically does not know if it
    comes from a proxy, reverse proxy, or the actual client. Setting
    `http.client_ip` when it&#39;s the same as `net.peer.ip` means that
    one is at least somewhat confident that the address is not that of
    the closest proxy.
      */
    HTTP_CLIENT_IP: "http.client_ip",
    /**
     * The keys in the `RequestItems` object field.
     */
    AWS_DYNAMODB_TABLE_NAMES: "aws.dynamodb.table_names",
    /**
     * The JSON-serialized value of each item in the `ConsumedCapacity` response field.
     */
    AWS_DYNAMODB_CONSUMED_CAPACITY: "aws.dynamodb.consumed_capacity",
    /**
     * The JSON-serialized value of the `ItemCollectionMetrics` response field.
     */
    AWS_DYNAMODB_ITEM_COLLECTION_METRICS: "aws.dynamodb.item_collection_metrics",
    /**
     * The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter.
     */
    AWS_DYNAMODB_PROVISIONED_READ_CAPACITY: "aws.dynamodb.provisioned_read_capacity",
    /**
     * The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter.
     */
    AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY: "aws.dynamodb.provisioned_write_capacity",
    /**
     * The value of the `ConsistentRead` request parameter.
     */
    AWS_DYNAMODB_CONSISTENT_READ: "aws.dynamodb.consistent_read",
    /**
     * The value of the `ProjectionExpression` request parameter.
     */
    AWS_DYNAMODB_PROJECTION: "aws.dynamodb.projection",
    /**
     * The value of the `Limit` request parameter.
     */
    AWS_DYNAMODB_LIMIT: "aws.dynamodb.limit",
    /**
     * The value of the `AttributesToGet` request parameter.
     */
    AWS_DYNAMODB_ATTRIBUTES_TO_GET: "aws.dynamodb.attributes_to_get",
    /**
     * The value of the `IndexName` request parameter.
     */
    AWS_DYNAMODB_INDEX_NAME: "aws.dynamodb.index_name",
    /**
     * The value of the `Select` request parameter.
     */
    AWS_DYNAMODB_SELECT: "aws.dynamodb.select",
    /**
     * The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field.
     */
    AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES: "aws.dynamodb.global_secondary_indexes",
    /**
     * The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field.
     */
    AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES: "aws.dynamodb.local_secondary_indexes",
    /**
     * The value of the `ExclusiveStartTableName` request parameter.
     */
    AWS_DYNAMODB_EXCLUSIVE_START_TABLE: "aws.dynamodb.exclusive_start_table",
    /**
     * The the number of items in the `TableNames` response parameter.
     */
    AWS_DYNAMODB_TABLE_COUNT: "aws.dynamodb.table_count",
    /**
     * The value of the `ScanIndexForward` request parameter.
     */
    AWS_DYNAMODB_SCAN_FORWARD: "aws.dynamodb.scan_forward",
    /**
     * The value of the `Segment` request parameter.
     */
    AWS_DYNAMODB_SEGMENT: "aws.dynamodb.segment",
    /**
     * The value of the `TotalSegments` request parameter.
     */
    AWS_DYNAMODB_TOTAL_SEGMENTS: "aws.dynamodb.total_segments",
    /**
     * The value of the `Count` response parameter.
     */
    AWS_DYNAMODB_COUNT: "aws.dynamodb.count",
    /**
     * The value of the `ScannedCount` response parameter.
     */
    AWS_DYNAMODB_SCANNED_COUNT: "aws.dynamodb.scanned_count",
    /**
     * The JSON-serialized value of each item in the `AttributeDefinitions` request field.
     */
    AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS: "aws.dynamodb.attribute_definitions",
    /**
     * The JSON-serialized value of each item in the the `GlobalSecondaryIndexUpdates` request field.
     */
    AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES: "aws.dynamodb.global_secondary_index_updates",
    /**
     * A string identifying the messaging system.
     */
    MESSAGING_SYSTEM: "messaging.system",
    /**
     * The message destination name. This might be equal to the span name but is required nevertheless.
     */
    MESSAGING_DESTINATION: "messaging.destination",
    /**
     * The kind of message destination.
     */
    MESSAGING_DESTINATION_KIND: "messaging.destination_kind",
    /**
     * A boolean that is true if the message destination is temporary.
     */
    MESSAGING_TEMP_DESTINATION: "messaging.temp_destination",
    /**
     * The name of the transport protocol.
     */
    MESSAGING_PROTOCOL: "messaging.protocol",
    /**
     * The version of the transport protocol.
     */
    MESSAGING_PROTOCOL_VERSION: "messaging.protocol_version",
    /**
     * Connection string.
     */
    MESSAGING_URL: "messaging.url",
    /**
     * A value used by the messaging system as an identifier for the message, represented as a string.
     */
    MESSAGING_MESSAGE_ID: "messaging.message_id",
    /**
     * The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called &#34;Correlation ID&#34;.
     */
    MESSAGING_CONVERSATION_ID: "messaging.conversation_id",
    /**
     * The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported.
     */
    MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES: "messaging.message_payload_size_bytes",
    /**
     * The compressed size of the message payload in bytes.
     */
    MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES: "messaging.message_payload_compressed_size_bytes",
    /**
     * A string identifying the kind of message consumption as defined in the [Operation names](#operation-names) section above. If the operation is &#34;send&#34;, this attribute MUST NOT be set, since the operation can be inferred from the span kind in that case.
     */
    MESSAGING_OPERATION: "messaging.operation",
    /**
     * The identifier for the consumer receiving a message. For Kafka, set it to `{messaging.kafka.consumer_group} - {messaging.kafka.client_id}`, if both are present, or only `messaging.kafka.consumer_group`. For brokers, such as RabbitMQ and Artemis, set it to the `client_id` of the client consuming the message.
     */
    MESSAGING_CONSUMER_ID: "messaging.consumer_id",
    /**
     * RabbitMQ message routing key.
     */
    MESSAGING_RABBITMQ_ROUTING_KEY: "messaging.rabbitmq.routing_key",
    /**
     * Message keys in Kafka are used for grouping alike messages to ensure they&#39;re processed on the same partition. They differ from `messaging.message_id` in that they&#39;re not unique. If the key is `null`, the attribute MUST NOT be set.
     *
     * Note: If the key type is not string, it&#39;s string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don&#39;t include its value.
     */
    MESSAGING_KAFKA_MESSAGE_KEY: "messaging.kafka.message_key",
    /**
     * Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers.
     */
    MESSAGING_KAFKA_CONSUMER_GROUP: "messaging.kafka.consumer_group",
    /**
     * Client Id for the Consumer or Producer that is handling the message.
     */
    MESSAGING_KAFKA_CLIENT_ID: "messaging.kafka.client_id",
    /**
     * Partition the message is sent to.
     */
    MESSAGING_KAFKA_PARTITION: "messaging.kafka.partition",
    /**
     * A boolean that is true if the message is a tombstone.
     */
    MESSAGING_KAFKA_TOMBSTONE: "messaging.kafka.tombstone",
    /**
     * A string identifying the remoting system.
     */
    RPC_SYSTEM: "rpc.system",
    /**
     * The full (logical) name of the service being called, including its package name, if applicable.
     *
     * Note: This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side).
     */
    RPC_SERVICE: "rpc.service",
    /**
     * The name of the (logical) method being called, must be equal to the $method part in the span name.
     *
     * Note: This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side).
     */
    RPC_METHOD: "rpc.method",
    /**
     * The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.
     */
    RPC_GRPC_STATUS_CODE: "rpc.grpc.status_code",
    /**
     * Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 does not specify this, the value can be omitted.
     */
    RPC_JSONRPC_VERSION: "rpc.jsonrpc.version",
    /**
     * `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification.
     */
    RPC_JSONRPC_REQUEST_ID: "rpc.jsonrpc.request_id",
    /**
     * `error.code` property of response if it is an error response.
     */
    RPC_JSONRPC_ERROR_CODE: "rpc.jsonrpc.error_code",
    /**
     * `error.message` property of response if it is an error response.
     */
    RPC_JSONRPC_ERROR_MESSAGE: "rpc.jsonrpc.error_message",
    /**
     * Whether this is a received or sent message.
     */
    MESSAGE_TYPE: "message.type",
    /**
     * MUST be calculated as two different counters starting from `1` one for sent messages and one for received message.
     *
     * Note: This way we guarantee that the values will be consistent between different implementations.
     */
    MESSAGE_ID: "message.id",
    /**
     * Compressed size of the message in bytes.
     */
    MESSAGE_COMPRESSED_SIZE: "message.compressed_size",
    /**
     * Uncompressed size of the message in bytes.
     */
    MESSAGE_UNCOMPRESSED_SIZE: "message.uncompressed_size"
  };

  // node_modules/@opentelemetry/semantic-conventions/build/esm/resource/SemanticResourceAttributes.js
  var SemanticResourceAttributes = {
    /**
     * Name of the cloud provider.
     */
    CLOUD_PROVIDER: "cloud.provider",
    /**
     * The cloud account ID the resource is assigned to.
     */
    CLOUD_ACCOUNT_ID: "cloud.account.id",
    /**
     * The geographical region the resource is running. Refer to your provider&#39;s docs to see the available regions, for example [Alibaba Cloud regions](https://www.alibabacloud.com/help/doc-detail/40654.htm), [AWS regions](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/), [Azure regions](https://azure.microsoft.com/en-us/global-infrastructure/geographies/), or [Google Cloud regions](https://cloud.google.com/about/locations).
     */
    CLOUD_REGION: "cloud.region",
    /**
     * Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running.
     *
     * Note: Availability zones are called &#34;zones&#34; on Alibaba Cloud and Google Cloud.
     */
    CLOUD_AVAILABILITY_ZONE: "cloud.availability_zone",
    /**
     * The cloud platform in use.
     *
     * Note: The prefix of the service SHOULD match the one specified in `cloud.provider`.
     */
    CLOUD_PLATFORM: "cloud.platform",
    /**
     * The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html).
     */
    AWS_ECS_CONTAINER_ARN: "aws.ecs.container.arn",
    /**
     * The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html).
     */
    AWS_ECS_CLUSTER_ARN: "aws.ecs.cluster.arn",
    /**
     * The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.
     */
    AWS_ECS_LAUNCHTYPE: "aws.ecs.launchtype",
    /**
     * The ARN of an [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html).
     */
    AWS_ECS_TASK_ARN: "aws.ecs.task.arn",
    /**
     * The task definition family this task definition is a member of.
     */
    AWS_ECS_TASK_FAMILY: "aws.ecs.task.family",
    /**
     * The revision for this task definition.
     */
    AWS_ECS_TASK_REVISION: "aws.ecs.task.revision",
    /**
     * The ARN of an EKS cluster.
     */
    AWS_EKS_CLUSTER_ARN: "aws.eks.cluster.arn",
    /**
     * The name(s) of the AWS log group(s) an application is writing to.
     *
     * Note: Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group.
     */
    AWS_LOG_GROUP_NAMES: "aws.log.group.names",
    /**
     * The Amazon Resource Name(s) (ARN) of the AWS log group(s).
     *
     * Note: See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format).
     */
    AWS_LOG_GROUP_ARNS: "aws.log.group.arns",
    /**
     * The name(s) of the AWS log stream(s) an application is writing to.
     */
    AWS_LOG_STREAM_NAMES: "aws.log.stream.names",
    /**
     * The ARN(s) of the AWS log stream(s).
     *
     * Note: See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream.
     */
    AWS_LOG_STREAM_ARNS: "aws.log.stream.arns",
    /**
     * Container name.
     */
    CONTAINER_NAME: "container.name",
    /**
     * Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated.
     */
    CONTAINER_ID: "container.id",
    /**
     * The container runtime managing this container.
     */
    CONTAINER_RUNTIME: "container.runtime",
    /**
     * Name of the image the container was built on.
     */
    CONTAINER_IMAGE_NAME: "container.image.name",
    /**
     * Container image tag.
     */
    CONTAINER_IMAGE_TAG: "container.image.tag",
    /**
     * Name of the [deployment environment](https://en.wikipedia.org/wiki/Deployment_environment) (aka deployment tier).
     */
    DEPLOYMENT_ENVIRONMENT: "deployment.environment",
    /**
     * A unique identifier representing the device.
     *
     * Note: The device identifier MUST only be defined using the values outlined below. This value is not an advertising identifier and MUST NOT be used as such. On iOS (Swift or Objective-C), this value MUST be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value MUST be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence.
     */
    DEVICE_ID: "device.id",
    /**
     * The model identifier for the device.
     *
     * Note: It&#39;s recommended this value represents a machine readable version of the model identifier rather than the market or consumer-friendly name of the device.
     */
    DEVICE_MODEL_IDENTIFIER: "device.model.identifier",
    /**
     * The marketing name for the device model.
     *
     * Note: It&#39;s recommended this value represents a human readable version of the device model rather than a machine readable alternative.
     */
    DEVICE_MODEL_NAME: "device.model.name",
    /**
     * The name of the single function that this runtime instance executes.
     *
     * Note: This is the name of the function as configured/deployed on the FaaS platform and is usually different from the name of the callback function (which may be stored in the [`code.namespace`/`code.function`](../../trace/semantic_conventions/span-general.md#source-code-attributes) span attributes).
     */
    FAAS_NAME: "faas.name",
    /**
      * The unique ID of the single function that this runtime instance executes.
      *
      * Note: Depending on the cloud provider, use:
    
    * **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
    Take care not to use the &#34;invoked ARN&#34; directly but replace any
    [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html) with the resolved function version, as the same runtime instance may be invokable with multiple
    different aliases.
    * **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
    * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/en-us/rest/api/resources/resources/get-by-id).
    
    On some providers, it may not be possible to determine the full ID at startup,
    which is why this field cannot be made required. For example, on AWS the account ID
    part of the ARN is not available without calling another AWS API
    which may be deemed too slow for a short-running lambda function.
    As an alternative, consider setting `faas.id` as a span attribute instead.
      */
    FAAS_ID: "faas.id",
    /**
      * The immutable version of the function being executed.
      *
      * Note: Depending on the cloud provider and platform, use:
    
    * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
      (an integer represented as a decimal string).
    * **Google Cloud Run:** The [revision](https://cloud.google.com/run/docs/managing/revisions)
      (i.e., the function name plus the revision suffix).
    * **Google Cloud Functions:** The value of the
      [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
    * **Azure Functions:** Not applicable. Do not set this attribute.
      */
    FAAS_VERSION: "faas.version",
    /**
     * The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.
     *
     * Note: * **AWS Lambda:** Use the (full) log stream name.
     */
    FAAS_INSTANCE: "faas.instance",
    /**
     * The amount of memory available to the serverless function in MiB.
     *
     * Note: It&#39;s recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information.
     */
    FAAS_MAX_MEMORY: "faas.max_memory",
    /**
     * Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider.
     */
    HOST_ID: "host.id",
    /**
     * Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user.
     */
    HOST_NAME: "host.name",
    /**
     * Type of host. For Cloud, this must be the machine type.
     */
    HOST_TYPE: "host.type",
    /**
     * The CPU architecture the host system is running on.
     */
    HOST_ARCH: "host.arch",
    /**
     * Name of the VM image or OS install the host was instantiated from.
     */
    HOST_IMAGE_NAME: "host.image.name",
    /**
     * VM image ID. For Cloud, this value is from the provider.
     */
    HOST_IMAGE_ID: "host.image.id",
    /**
     * The version string of the VM image as defined in [Version SpanAttributes](README.md#version-attributes).
     */
    HOST_IMAGE_VERSION: "host.image.version",
    /**
     * The name of the cluster.
     */
    K8S_CLUSTER_NAME: "k8s.cluster.name",
    /**
     * The name of the Node.
     */
    K8S_NODE_NAME: "k8s.node.name",
    /**
     * The UID of the Node.
     */
    K8S_NODE_UID: "k8s.node.uid",
    /**
     * The name of the namespace that the pod is running in.
     */
    K8S_NAMESPACE_NAME: "k8s.namespace.name",
    /**
     * The UID of the Pod.
     */
    K8S_POD_UID: "k8s.pod.uid",
    /**
     * The name of the Pod.
     */
    K8S_POD_NAME: "k8s.pod.name",
    /**
     * The name of the Container in a Pod template.
     */
    K8S_CONTAINER_NAME: "k8s.container.name",
    /**
     * The UID of the ReplicaSet.
     */
    K8S_REPLICASET_UID: "k8s.replicaset.uid",
    /**
     * The name of the ReplicaSet.
     */
    K8S_REPLICASET_NAME: "k8s.replicaset.name",
    /**
     * The UID of the Deployment.
     */
    K8S_DEPLOYMENT_UID: "k8s.deployment.uid",
    /**
     * The name of the Deployment.
     */
    K8S_DEPLOYMENT_NAME: "k8s.deployment.name",
    /**
     * The UID of the StatefulSet.
     */
    K8S_STATEFULSET_UID: "k8s.statefulset.uid",
    /**
     * The name of the StatefulSet.
     */
    K8S_STATEFULSET_NAME: "k8s.statefulset.name",
    /**
     * The UID of the DaemonSet.
     */
    K8S_DAEMONSET_UID: "k8s.daemonset.uid",
    /**
     * The name of the DaemonSet.
     */
    K8S_DAEMONSET_NAME: "k8s.daemonset.name",
    /**
     * The UID of the Job.
     */
    K8S_JOB_UID: "k8s.job.uid",
    /**
     * The name of the Job.
     */
    K8S_JOB_NAME: "k8s.job.name",
    /**
     * The UID of the CronJob.
     */
    K8S_CRONJOB_UID: "k8s.cronjob.uid",
    /**
     * The name of the CronJob.
     */
    K8S_CRONJOB_NAME: "k8s.cronjob.name",
    /**
     * The operating system type.
     */
    OS_TYPE: "os.type",
    /**
     * Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands.
     */
    OS_DESCRIPTION: "os.description",
    /**
     * Human readable operating system name.
     */
    OS_NAME: "os.name",
    /**
     * The version string of the operating system as defined in [Version SpanAttributes](../../resource/semantic_conventions/README.md#version-attributes).
     */
    OS_VERSION: "os.version",
    /**
     * Process identifier (PID).
     */
    PROCESS_PID: "process.pid",
    /**
     * The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`.
     */
    PROCESS_EXECUTABLE_NAME: "process.executable.name",
    /**
     * The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`.
     */
    PROCESS_EXECUTABLE_PATH: "process.executable.path",
    /**
     * The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`.
     */
    PROCESS_COMMAND: "process.command",
    /**
     * The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead.
     */
    PROCESS_COMMAND_LINE: "process.command_line",
    /**
     * All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`.
     */
    PROCESS_COMMAND_ARGS: "process.command_args",
    /**
     * The username of the user that owns the process.
     */
    PROCESS_OWNER: "process.owner",
    /**
     * The name of the runtime of this process. For compiled native binaries, this SHOULD be the name of the compiler.
     */
    PROCESS_RUNTIME_NAME: "process.runtime.name",
    /**
     * The version of the runtime of this process, as returned by the runtime without modification.
     */
    PROCESS_RUNTIME_VERSION: "process.runtime.version",
    /**
     * An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment.
     */
    PROCESS_RUNTIME_DESCRIPTION: "process.runtime.description",
    /**
     * Logical name of the service.
     *
     * Note: MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs MUST fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value MUST be set to `unknown_service`.
     */
    SERVICE_NAME: "service.name",
    /**
     * A namespace for `service.name`.
     *
     * Note: A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace.
     */
    SERVICE_NAMESPACE: "service.namespace",
    /**
     * The string ID of the service instance.
     *
     * Note: MUST be unique for each instance of the same `service.namespace,service.name` pair (in other words `service.namespace,service.name,service.instance.id` triplet MUST be globally unique). The ID helps to distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled service). It is preferable for the ID to be persistent and stay the same for the lifetime of the service instance, however it is acceptable that the ID is ephemeral and changes during important lifetime events for the service (e.g. service restarts). If the service has no inherent unique ID that can be used as the value of this attribute it is recommended to generate a random Version 1 or Version 4 RFC 4122 UUID (services aiming for reproducible UUIDs may also use Version 5, see RFC 4122 for more recommendations).
     */
    SERVICE_INSTANCE_ID: "service.instance.id",
    /**
     * The version string of the service API or implementation.
     */
    SERVICE_VERSION: "service.version",
    /**
     * The name of the telemetry SDK as defined above.
     */
    TELEMETRY_SDK_NAME: "telemetry.sdk.name",
    /**
     * The language of the telemetry SDK.
     */
    TELEMETRY_SDK_LANGUAGE: "telemetry.sdk.language",
    /**
     * The version string of the telemetry SDK.
     */
    TELEMETRY_SDK_VERSION: "telemetry.sdk.version",
    /**
     * The version string of the auto instrumentation agent, if used.
     */
    TELEMETRY_AUTO_VERSION: "telemetry.auto.version",
    /**
     * The name of the web engine.
     */
    WEBENGINE_NAME: "webengine.name",
    /**
     * The version of the web engine.
     */
    WEBENGINE_VERSION: "webengine.version",
    /**
     * Additional description of the web engine (e.g. detailed version and edition information).
     */
    WEBENGINE_DESCRIPTION: "webengine.description"
  };
  var TelemetrySdkLanguageValues = {
    /** cpp. */
    CPP: "cpp",
    /** dotnet. */
    DOTNET: "dotnet",
    /** erlang. */
    ERLANG: "erlang",
    /** go. */
    GO: "go",
    /** java. */
    JAVA: "java",
    /** nodejs. */
    NODEJS: "nodejs",
    /** php. */
    PHP: "php",
    /** python. */
    PYTHON: "python",
    /** ruby. */
    RUBY: "ruby",
    /** webjs. */
    WEBJS: "webjs"
  };

  // node_modules/@opentelemetry/core/build/esm/platform/browser/sdk-info.js
  var _a;
  var SDK_INFO = (_a = {}, _a[SemanticResourceAttributes.TELEMETRY_SDK_NAME] = "opentelemetry", _a[SemanticResourceAttributes.PROCESS_RUNTIME_NAME] = "browser", _a[SemanticResourceAttributes.TELEMETRY_SDK_LANGUAGE] = TelemetrySdkLanguageValues.WEBJS, _a[SemanticResourceAttributes.TELEMETRY_SDK_VERSION] = VERSION2, _a);

  // node_modules/@opentelemetry/core/build/esm/platform/browser/timer-util.js
  function unrefTimer(_timer) {
  }

  // node_modules/@opentelemetry/core/build/esm/common/time.js
  var NANOSECOND_DIGITS = 9;
  var NANOSECOND_DIGITS_IN_MILLIS = 6;
  var MILLISECONDS_TO_NANOSECONDS = Math.pow(10, NANOSECOND_DIGITS_IN_MILLIS);
  var SECOND_TO_NANOSECONDS = Math.pow(10, NANOSECOND_DIGITS);
  function millisToHrTime(epochMillis) {
    var epochSeconds = epochMillis / 1e3;
    var seconds = Math.trunc(epochSeconds);
    var nanos = Math.round(epochMillis % 1e3 * MILLISECONDS_TO_NANOSECONDS);
    return [seconds, nanos];
  }
  function getTimeOrigin() {
    var timeOrigin = otperformance.timeOrigin;
    if (typeof timeOrigin !== "number") {
      var perf = otperformance;
      timeOrigin = perf.timing && perf.timing.fetchStart;
    }
    return timeOrigin;
  }
  function hrTime(performanceNow) {
    var timeOrigin = millisToHrTime(getTimeOrigin());
    var now = millisToHrTime(typeof performanceNow === "number" ? performanceNow : otperformance.now());
    return addHrTimes(timeOrigin, now);
  }
  function timeInputToHrTime(time) {
    if (isTimeInputHrTime(time)) {
      return time;
    } else if (typeof time === "number") {
      if (time < getTimeOrigin()) {
        return hrTime(time);
      } else {
        return millisToHrTime(time);
      }
    } else if (time instanceof Date) {
      return millisToHrTime(time.getTime());
    } else {
      throw TypeError("Invalid input type");
    }
  }
  function hrTimeDuration(startTime, endTime) {
    var seconds = endTime[0] - startTime[0];
    var nanos = endTime[1] - startTime[1];
    if (nanos < 0) {
      seconds -= 1;
      nanos += SECOND_TO_NANOSECONDS;
    }
    return [seconds, nanos];
  }
  function hrTimeToNanoseconds(time) {
    return time[0] * SECOND_TO_NANOSECONDS + time[1];
  }
  function hrTimeToMicroseconds(time) {
    return Math.round(time[0] * 1e6 + time[1] / 1e3);
  }
  function isTimeInputHrTime(value) {
    return Array.isArray(value) && value.length === 2 && typeof value[0] === "number" && typeof value[1] === "number";
  }
  function isTimeInput(value) {
    return isTimeInputHrTime(value) || typeof value === "number" || value instanceof Date;
  }
  function addHrTimes(time1, time2) {
    var out = [time1[0] + time2[0], time1[1] + time2[1]];
    if (out[1] >= SECOND_TO_NANOSECONDS) {
      out[1] -= SECOND_TO_NANOSECONDS;
      out[0] += 1;
    }
    return out;
  }

  // node_modules/@opentelemetry/core/build/esm/ExportResult.js
  var ExportResultCode;
  (function(ExportResultCode2) {
    ExportResultCode2[ExportResultCode2["SUCCESS"] = 0] = "SUCCESS";
    ExportResultCode2[ExportResultCode2["FAILED"] = 1] = "FAILED";
  })(ExportResultCode || (ExportResultCode = {}));

  // node_modules/@opentelemetry/core/build/esm/propagation/composite.js
  var __values3 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var CompositePropagator = (
    /** @class */
    function() {
      function CompositePropagator2(config) {
        if (config === void 0) {
          config = {};
        }
        var _a2;
        this._propagators = (_a2 = config.propagators) !== null && _a2 !== void 0 ? _a2 : [];
        this._fields = Array.from(new Set(this._propagators.map(function(p2) {
          return typeof p2.fields === "function" ? p2.fields() : [];
        }).reduce(function(x2, y2) {
          return x2.concat(y2);
        }, [])));
      }
      CompositePropagator2.prototype.inject = function(context2, carrier, setter) {
        var e_1, _a2;
        try {
          for (var _b = __values3(this._propagators), _c = _b.next(); !_c.done; _c = _b.next()) {
            var propagator = _c.value;
            try {
              propagator.inject(context2, carrier, setter);
            } catch (err) {
              diag2.warn("Failed to inject with " + propagator.constructor.name + ". Err: " + err.message);
            }
          }
        } catch (e_1_1) {
          e_1 = { error: e_1_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_1)
              throw e_1.error;
          }
        }
      };
      CompositePropagator2.prototype.extract = function(context2, carrier, getter) {
        return this._propagators.reduce(function(ctx, propagator) {
          try {
            return propagator.extract(ctx, carrier, getter);
          } catch (err) {
            diag2.warn("Failed to inject with " + propagator.constructor.name + ". Err: " + err.message);
          }
          return ctx;
        }, context2);
      };
      CompositePropagator2.prototype.fields = function() {
        return this._fields.slice();
      };
      return CompositePropagator2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/internal/validators.js
  var VALID_KEY_CHAR_RANGE = "[_0-9a-z-*/]";
  var VALID_KEY = "[a-z]" + VALID_KEY_CHAR_RANGE + "{0,255}";
  var VALID_VENDOR_KEY = "[a-z0-9]" + VALID_KEY_CHAR_RANGE + "{0,240}@[a-z]" + VALID_KEY_CHAR_RANGE + "{0,13}";
  var VALID_KEY_REGEX = new RegExp("^(?:" + VALID_KEY + "|" + VALID_VENDOR_KEY + ")$");
  var VALID_VALUE_BASE_REGEX = /^[ -~]{0,255}[!-~]$/;
  var INVALID_VALUE_COMMA_EQUAL_REGEX = /,|=/;
  function validateKey(key) {
    return VALID_KEY_REGEX.test(key);
  }
  function validateValue(value) {
    return VALID_VALUE_BASE_REGEX.test(value) && !INVALID_VALUE_COMMA_EQUAL_REGEX.test(value);
  }

  // node_modules/@opentelemetry/core/build/esm/trace/TraceState.js
  var MAX_TRACE_STATE_ITEMS = 32;
  var MAX_TRACE_STATE_LEN = 512;
  var LIST_MEMBERS_SEPARATOR = ",";
  var LIST_MEMBER_KEY_VALUE_SPLITTER = "=";
  var TraceState = (
    /** @class */
    function() {
      function TraceState2(rawTraceState) {
        this._internalState = /* @__PURE__ */ new Map();
        if (rawTraceState)
          this._parse(rawTraceState);
      }
      TraceState2.prototype.set = function(key, value) {
        var traceState = this._clone();
        if (traceState._internalState.has(key)) {
          traceState._internalState.delete(key);
        }
        traceState._internalState.set(key, value);
        return traceState;
      };
      TraceState2.prototype.unset = function(key) {
        var traceState = this._clone();
        traceState._internalState.delete(key);
        return traceState;
      };
      TraceState2.prototype.get = function(key) {
        return this._internalState.get(key);
      };
      TraceState2.prototype.serialize = function() {
        var _this = this;
        return this._keys().reduce(function(agg, key) {
          agg.push(key + LIST_MEMBER_KEY_VALUE_SPLITTER + _this.get(key));
          return agg;
        }, []).join(LIST_MEMBERS_SEPARATOR);
      };
      TraceState2.prototype._parse = function(rawTraceState) {
        if (rawTraceState.length > MAX_TRACE_STATE_LEN)
          return;
        this._internalState = rawTraceState.split(LIST_MEMBERS_SEPARATOR).reverse().reduce(function(agg, part) {
          var listMember = part.trim();
          var i2 = listMember.indexOf(LIST_MEMBER_KEY_VALUE_SPLITTER);
          if (i2 !== -1) {
            var key = listMember.slice(0, i2);
            var value = listMember.slice(i2 + 1, part.length);
            if (validateKey(key) && validateValue(value)) {
              agg.set(key, value);
            } else {
            }
          }
          return agg;
        }, /* @__PURE__ */ new Map());
        if (this._internalState.size > MAX_TRACE_STATE_ITEMS) {
          this._internalState = new Map(Array.from(this._internalState.entries()).reverse().slice(0, MAX_TRACE_STATE_ITEMS));
        }
      };
      TraceState2.prototype._keys = function() {
        return Array.from(this._internalState.keys()).reverse();
      };
      TraceState2.prototype._clone = function() {
        var traceState = new TraceState2();
        traceState._internalState = new Map(this._internalState);
        return traceState;
      };
      return TraceState2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/trace/W3CTraceContextPropagator.js
  var TRACE_PARENT_HEADER = "traceparent";
  var TRACE_STATE_HEADER = "tracestate";
  var VERSION3 = "00";
  var VERSION_PART = "(?!ff)[\\da-f]{2}";
  var TRACE_ID_PART = "(?![0]{32})[\\da-f]{32}";
  var PARENT_ID_PART = "(?![0]{16})[\\da-f]{16}";
  var FLAGS_PART = "[\\da-f]{2}";
  var TRACE_PARENT_REGEX = new RegExp("^\\s?(" + VERSION_PART + ")-(" + TRACE_ID_PART + ")-(" + PARENT_ID_PART + ")-(" + FLAGS_PART + ")(-.*)?\\s?$");
  function parseTraceParent(traceParent) {
    var match = TRACE_PARENT_REGEX.exec(traceParent);
    if (!match)
      return null;
    if (match[1] === "00" && match[5])
      return null;
    return {
      traceId: match[2],
      spanId: match[3],
      traceFlags: parseInt(match[4], 16)
    };
  }
  var W3CTraceContextPropagator = (
    /** @class */
    function() {
      function W3CTraceContextPropagator2() {
      }
      W3CTraceContextPropagator2.prototype.inject = function(context2, carrier, setter) {
        var spanContext = trace.getSpanContext(context2);
        if (!spanContext || isTracingSuppressed(context2) || !isSpanContextValid(spanContext))
          return;
        var traceParent = VERSION3 + "-" + spanContext.traceId + "-" + spanContext.spanId + "-0" + Number(spanContext.traceFlags || TraceFlags.NONE).toString(16);
        setter.set(carrier, TRACE_PARENT_HEADER, traceParent);
        if (spanContext.traceState) {
          setter.set(carrier, TRACE_STATE_HEADER, spanContext.traceState.serialize());
        }
      };
      W3CTraceContextPropagator2.prototype.extract = function(context2, carrier, getter) {
        var traceParentHeader = getter.get(carrier, TRACE_PARENT_HEADER);
        if (!traceParentHeader)
          return context2;
        var traceParent = Array.isArray(traceParentHeader) ? traceParentHeader[0] : traceParentHeader;
        if (typeof traceParent !== "string")
          return context2;
        var spanContext = parseTraceParent(traceParent);
        if (!spanContext)
          return context2;
        spanContext.isRemote = true;
        var traceStateHeader = getter.get(carrier, TRACE_STATE_HEADER);
        if (traceStateHeader) {
          var state = Array.isArray(traceStateHeader) ? traceStateHeader.join(",") : traceStateHeader;
          spanContext.traceState = new TraceState(typeof state === "string" ? state : void 0);
        }
        return trace.setSpanContext(context2, spanContext);
      };
      W3CTraceContextPropagator2.prototype.fields = function() {
        return [TRACE_PARENT_HEADER, TRACE_STATE_HEADER];
      };
      return W3CTraceContextPropagator2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/utils/lodash.merge.js
  var objectTag = "[object Object]";
  var nullTag = "[object Null]";
  var undefinedTag = "[object Undefined]";
  var funcProto = Function.prototype;
  var funcToString = funcProto.toString;
  var objectCtorString = funcToString.call(Object);
  var getPrototype = overArg(Object.getPrototypeOf, Object);
  var objectProto = Object.prototype;
  var hasOwnProperty = objectProto.hasOwnProperty;
  var symToStringTag = Symbol ? Symbol.toStringTag : void 0;
  var nativeObjectToString = objectProto.toString;
  function overArg(func, transform) {
    return function(arg) {
      return func(transform(arg));
    };
  }
  function isPlainObject(value) {
    if (!isObjectLike(value) || baseGetTag(value) !== objectTag) {
      return false;
    }
    var proto = getPrototype(value);
    if (proto === null) {
      return true;
    }
    var Ctor = hasOwnProperty.call(proto, "constructor") && proto.constructor;
    return typeof Ctor == "function" && Ctor instanceof Ctor && funcToString.call(Ctor) === objectCtorString;
  }
  function isObjectLike(value) {
    return value != null && typeof value == "object";
  }
  function baseGetTag(value) {
    if (value == null) {
      return value === void 0 ? undefinedTag : nullTag;
    }
    return symToStringTag && symToStringTag in Object(value) ? getRawTag(value) : objectToString(value);
  }
  function getRawTag(value) {
    var isOwn = hasOwnProperty.call(value, symToStringTag), tag = value[symToStringTag];
    var unmasked = false;
    try {
      value[symToStringTag] = void 0;
      unmasked = true;
    } catch (e2) {
    }
    var result = nativeObjectToString.call(value);
    if (unmasked) {
      if (isOwn) {
        value[symToStringTag] = tag;
      } else {
        delete value[symToStringTag];
      }
    }
    return result;
  }
  function objectToString(value) {
    return nativeObjectToString.call(value);
  }

  // node_modules/@opentelemetry/core/build/esm/utils/merge.js
  var MAX_LEVEL = 20;
  function merge() {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
      args[_i] = arguments[_i];
    }
    var result = args.shift();
    var objects = /* @__PURE__ */ new WeakMap();
    while (args.length > 0) {
      result = mergeTwoObjects(result, args.shift(), 0, objects);
    }
    return result;
  }
  function takeValue(value) {
    if (isArray(value)) {
      return value.slice();
    }
    return value;
  }
  function mergeTwoObjects(one, two, level, objects) {
    if (level === void 0) {
      level = 0;
    }
    var result;
    if (level > MAX_LEVEL) {
      return void 0;
    }
    level++;
    if (isPrimitive(one) || isPrimitive(two) || isFunction(two)) {
      result = takeValue(two);
    } else if (isArray(one)) {
      result = one.slice();
      if (isArray(two)) {
        for (var i2 = 0, j2 = two.length; i2 < j2; i2++) {
          result.push(takeValue(two[i2]));
        }
      } else if (isObject(two)) {
        var keys = Object.keys(two);
        for (var i2 = 0, j2 = keys.length; i2 < j2; i2++) {
          var key = keys[i2];
          result[key] = takeValue(two[key]);
        }
      }
    } else if (isObject(one)) {
      if (isObject(two)) {
        if (!shouldMerge(one, two)) {
          return two;
        }
        result = Object.assign({}, one);
        var keys = Object.keys(two);
        for (var i2 = 0, j2 = keys.length; i2 < j2; i2++) {
          var key = keys[i2];
          var twoValue = two[key];
          if (isPrimitive(twoValue)) {
            if (typeof twoValue === "undefined") {
              delete result[key];
            } else {
              result[key] = twoValue;
            }
          } else {
            var obj1 = result[key];
            var obj2 = twoValue;
            if (wasObjectReferenced(one, key, objects) || wasObjectReferenced(two, key, objects)) {
              delete result[key];
            } else {
              if (isObject(obj1) && isObject(obj2)) {
                var arr1 = objects.get(obj1) || [];
                var arr2 = objects.get(obj2) || [];
                arr1.push({ obj: one, key });
                arr2.push({ obj: two, key });
                objects.set(obj1, arr1);
                objects.set(obj2, arr2);
              }
              result[key] = mergeTwoObjects(result[key], twoValue, level, objects);
            }
          }
        }
      } else {
        result = two;
      }
    }
    return result;
  }
  function wasObjectReferenced(obj, key, objects) {
    var arr = objects.get(obj[key]) || [];
    for (var i2 = 0, j2 = arr.length; i2 < j2; i2++) {
      var info = arr[i2];
      if (info.key === key && info.obj === obj) {
        return true;
      }
    }
    return false;
  }
  function isArray(value) {
    return Array.isArray(value);
  }
  function isFunction(value) {
    return typeof value === "function";
  }
  function isObject(value) {
    return !isPrimitive(value) && !isArray(value) && !isFunction(value) && typeof value === "object";
  }
  function isPrimitive(value) {
    return typeof value === "string" || typeof value === "number" || typeof value === "boolean" || typeof value === "undefined" || value instanceof Date || value instanceof RegExp || value === null;
  }
  function shouldMerge(one, two) {
    if (!isPlainObject(one) || !isPlainObject(two)) {
      return false;
    }
    return true;
  }

  // node_modules/@opentelemetry/core/build/esm/utils/url.js
  var __values4 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  function urlMatches(url, urlToMatch) {
    if (typeof urlToMatch === "string") {
      return url === urlToMatch;
    } else {
      return !!url.match(urlToMatch);
    }
  }
  function isUrlIgnored(url, ignoredUrls) {
    var e_1, _a2;
    if (!ignoredUrls) {
      return false;
    }
    try {
      for (var ignoredUrls_1 = __values4(ignoredUrls), ignoredUrls_1_1 = ignoredUrls_1.next(); !ignoredUrls_1_1.done; ignoredUrls_1_1 = ignoredUrls_1.next()) {
        var ignoreUrl = ignoredUrls_1_1.value;
        if (urlMatches(url, ignoreUrl)) {
          return true;
        }
      }
    } catch (e_1_1) {
      e_1 = { error: e_1_1 };
    } finally {
      try {
        if (ignoredUrls_1_1 && !ignoredUrls_1_1.done && (_a2 = ignoredUrls_1.return))
          _a2.call(ignoredUrls_1);
      } finally {
        if (e_1)
          throw e_1.error;
      }
    }
    return false;
  }

  // node_modules/@opentelemetry/core/build/esm/utils/promise.js
  var Deferred = (
    /** @class */
    function() {
      function Deferred2() {
        var _this = this;
        this._promise = new Promise(function(resolve, reject) {
          _this._resolve = resolve;
          _this._reject = reject;
        });
      }
      Object.defineProperty(Deferred2.prototype, "promise", {
        get: function() {
          return this._promise;
        },
        enumerable: false,
        configurable: true
      });
      Deferred2.prototype.resolve = function(val) {
        this._resolve(val);
      };
      Deferred2.prototype.reject = function(err) {
        this._reject(err);
      };
      return Deferred2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/utils/callback.js
  var __read8 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray5 = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var BindOnceFuture = (
    /** @class */
    function() {
      function BindOnceFuture2(_callback, _that) {
        this._callback = _callback;
        this._that = _that;
        this._isCalled = false;
        this._deferred = new Deferred();
      }
      Object.defineProperty(BindOnceFuture2.prototype, "isCalled", {
        get: function() {
          return this._isCalled;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(BindOnceFuture2.prototype, "promise", {
        get: function() {
          return this._deferred.promise;
        },
        enumerable: false,
        configurable: true
      });
      BindOnceFuture2.prototype.call = function() {
        var _a2;
        var _this = this;
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          args[_i] = arguments[_i];
        }
        if (!this._isCalled) {
          this._isCalled = true;
          try {
            Promise.resolve((_a2 = this._callback).call.apply(_a2, __spreadArray5([this._that], __read8(args), false))).then(function(val) {
              return _this._deferred.resolve(val);
            }, function(err) {
              return _this._deferred.reject(err);
            });
          } catch (err) {
            this._deferred.reject(err);
          }
        }
        return this._deferred.promise;
      };
      return BindOnceFuture2;
    }()
  );

  // node_modules/@opentelemetry/core/build/esm/internal/exporter.js
  function _export(exporter, arg) {
    return new Promise(function(resolve) {
      context.with(suppressTracing(context.active()), function() {
        exporter.export(arg, function(result) {
          resolve(result);
        });
      });
    });
  }

  // node_modules/@opentelemetry/core/build/esm/index.js
  var internal = {
    _export
  };

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/enums.js
  var ExceptionEventName = "exception";

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/Span.js
  var __values5 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var __read9 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var Span = (
    /** @class */
    function() {
      function Span2(parentTracer, context2, spanName, spanContext, kind, parentSpanId, links, startTime, _deprecatedClock) {
        if (links === void 0) {
          links = [];
        }
        this.attributes = {};
        this.links = [];
        this.events = [];
        this._droppedAttributesCount = 0;
        this._droppedEventsCount = 0;
        this._droppedLinksCount = 0;
        this.status = {
          code: SpanStatusCode.UNSET
        };
        this.endTime = [0, 0];
        this._ended = false;
        this._duration = [-1, -1];
        this.name = spanName;
        this._spanContext = spanContext;
        this.parentSpanId = parentSpanId;
        this.kind = kind;
        this.links = links;
        var now = Date.now();
        this._performanceStartTime = otperformance.now();
        this._performanceOffset = now - (this._performanceStartTime + getTimeOrigin());
        this._startTimeProvided = startTime != null;
        this.startTime = this._getTime(startTime !== null && startTime !== void 0 ? startTime : now);
        this.resource = parentTracer.resource;
        this.instrumentationLibrary = parentTracer.instrumentationLibrary;
        this._spanLimits = parentTracer.getSpanLimits();
        this._spanProcessor = parentTracer.getActiveSpanProcessor();
        this._spanProcessor.onStart(this, context2);
        this._attributeValueLengthLimit = this._spanLimits.attributeValueLengthLimit || 0;
      }
      Span2.prototype.spanContext = function() {
        return this._spanContext;
      };
      Span2.prototype.setAttribute = function(key, value) {
        if (value == null || this._isSpanEnded())
          return this;
        if (key.length === 0) {
          diag2.warn("Invalid attribute key: " + key);
          return this;
        }
        if (!isAttributeValue(value)) {
          diag2.warn("Invalid attribute value set for key: " + key);
          return this;
        }
        if (Object.keys(this.attributes).length >= this._spanLimits.attributeCountLimit && !Object.prototype.hasOwnProperty.call(this.attributes, key)) {
          this._droppedAttributesCount++;
          return this;
        }
        this.attributes[key] = this._truncateToSize(value);
        return this;
      };
      Span2.prototype.setAttributes = function(attributes) {
        var e_1, _a2;
        try {
          for (var _b = __values5(Object.entries(attributes)), _c = _b.next(); !_c.done; _c = _b.next()) {
            var _d = __read9(_c.value, 2), k2 = _d[0], v2 = _d[1];
            this.setAttribute(k2, v2);
          }
        } catch (e_1_1) {
          e_1 = { error: e_1_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_1)
              throw e_1.error;
          }
        }
        return this;
      };
      Span2.prototype.addEvent = function(name, attributesOrStartTime, timeStamp) {
        if (this._isSpanEnded())
          return this;
        if (this._spanLimits.eventCountLimit === 0) {
          diag2.warn("No events allowed.");
          this._droppedEventsCount++;
          return this;
        }
        if (this.events.length >= this._spanLimits.eventCountLimit) {
          diag2.warn("Dropping extra events.");
          this.events.shift();
          this._droppedEventsCount++;
        }
        if (isTimeInput(attributesOrStartTime)) {
          if (!isTimeInput(timeStamp)) {
            timeStamp = attributesOrStartTime;
          }
          attributesOrStartTime = void 0;
        }
        var attributes = sanitizeAttributes(attributesOrStartTime);
        this.events.push({
          name,
          attributes,
          time: this._getTime(timeStamp),
          droppedAttributesCount: 0
        });
        return this;
      };
      Span2.prototype.setStatus = function(status) {
        if (this._isSpanEnded())
          return this;
        this.status = status;
        return this;
      };
      Span2.prototype.updateName = function(name) {
        if (this._isSpanEnded())
          return this;
        this.name = name;
        return this;
      };
      Span2.prototype.end = function(endTime) {
        if (this._isSpanEnded()) {
          diag2.error(this.name + " " + this._spanContext.traceId + "-" + this._spanContext.spanId + " - You can only call end() on a span once.");
          return;
        }
        this._ended = true;
        this.endTime = this._getTime(endTime);
        this._duration = hrTimeDuration(this.startTime, this.endTime);
        if (this._duration[0] < 0) {
          diag2.warn("Inconsistent start and end time, startTime > endTime. Setting span duration to 0ms.", this.startTime, this.endTime);
          this.endTime = this.startTime.slice();
          this._duration = [0, 0];
        }
        this._spanProcessor.onEnd(this);
      };
      Span2.prototype._getTime = function(inp) {
        if (typeof inp === "number" && inp < otperformance.now()) {
          return hrTime(inp + this._performanceOffset);
        }
        if (typeof inp === "number") {
          return millisToHrTime(inp);
        }
        if (inp instanceof Date) {
          return millisToHrTime(inp.getTime());
        }
        if (isTimeInputHrTime(inp)) {
          return inp;
        }
        if (this._startTimeProvided) {
          return millisToHrTime(Date.now());
        }
        var msDuration = otperformance.now() - this._performanceStartTime;
        return addHrTimes(this.startTime, millisToHrTime(msDuration));
      };
      Span2.prototype.isRecording = function() {
        return this._ended === false;
      };
      Span2.prototype.recordException = function(exception, time) {
        var attributes = {};
        if (typeof exception === "string") {
          attributes[SemanticAttributes.EXCEPTION_MESSAGE] = exception;
        } else if (exception) {
          if (exception.code) {
            attributes[SemanticAttributes.EXCEPTION_TYPE] = exception.code.toString();
          } else if (exception.name) {
            attributes[SemanticAttributes.EXCEPTION_TYPE] = exception.name;
          }
          if (exception.message) {
            attributes[SemanticAttributes.EXCEPTION_MESSAGE] = exception.message;
          }
          if (exception.stack) {
            attributes[SemanticAttributes.EXCEPTION_STACKTRACE] = exception.stack;
          }
        }
        if (attributes[SemanticAttributes.EXCEPTION_TYPE] || attributes[SemanticAttributes.EXCEPTION_MESSAGE]) {
          this.addEvent(ExceptionEventName, attributes, time);
        } else {
          diag2.warn("Failed to record an exception " + exception);
        }
      };
      Object.defineProperty(Span2.prototype, "duration", {
        get: function() {
          return this._duration;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(Span2.prototype, "ended", {
        get: function() {
          return this._ended;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(Span2.prototype, "droppedAttributesCount", {
        get: function() {
          return this._droppedAttributesCount;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(Span2.prototype, "droppedEventsCount", {
        get: function() {
          return this._droppedEventsCount;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(Span2.prototype, "droppedLinksCount", {
        get: function() {
          return this._droppedLinksCount;
        },
        enumerable: false,
        configurable: true
      });
      Span2.prototype._isSpanEnded = function() {
        if (this._ended) {
          diag2.warn("Can not execute the operation on ended Span {traceId: " + this._spanContext.traceId + ", spanId: " + this._spanContext.spanId + "}");
        }
        return this._ended;
      };
      Span2.prototype._truncateToLimitUtil = function(value, limit) {
        if (value.length <= limit) {
          return value;
        }
        return value.substr(0, limit);
      };
      Span2.prototype._truncateToSize = function(value) {
        var _this = this;
        var limit = this._attributeValueLengthLimit;
        if (limit <= 0) {
          diag2.warn("Attribute value limit must be positive, got " + limit);
          return value;
        }
        if (typeof value === "string") {
          return this._truncateToLimitUtil(value, limit);
        }
        if (Array.isArray(value)) {
          return value.map(function(val) {
            return typeof val === "string" ? _this._truncateToLimitUtil(val, limit) : val;
          });
        }
        return value;
      };
      return Span2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/Sampler.js
  var SamplingDecision2;
  (function(SamplingDecision3) {
    SamplingDecision3[SamplingDecision3["NOT_RECORD"] = 0] = "NOT_RECORD";
    SamplingDecision3[SamplingDecision3["RECORD"] = 1] = "RECORD";
    SamplingDecision3[SamplingDecision3["RECORD_AND_SAMPLED"] = 2] = "RECORD_AND_SAMPLED";
  })(SamplingDecision2 || (SamplingDecision2 = {}));

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/sampler/AlwaysOffSampler.js
  var AlwaysOffSampler = (
    /** @class */
    function() {
      function AlwaysOffSampler2() {
      }
      AlwaysOffSampler2.prototype.shouldSample = function() {
        return {
          decision: SamplingDecision2.NOT_RECORD
        };
      };
      AlwaysOffSampler2.prototype.toString = function() {
        return "AlwaysOffSampler";
      };
      return AlwaysOffSampler2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/sampler/AlwaysOnSampler.js
  var AlwaysOnSampler = (
    /** @class */
    function() {
      function AlwaysOnSampler2() {
      }
      AlwaysOnSampler2.prototype.shouldSample = function() {
        return {
          decision: SamplingDecision2.RECORD_AND_SAMPLED
        };
      };
      AlwaysOnSampler2.prototype.toString = function() {
        return "AlwaysOnSampler";
      };
      return AlwaysOnSampler2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/sampler/ParentBasedSampler.js
  var ParentBasedSampler = (
    /** @class */
    function() {
      function ParentBasedSampler2(config) {
        var _a2, _b, _c, _d;
        this._root = config.root;
        if (!this._root) {
          globalErrorHandler(new Error("ParentBasedSampler must have a root sampler configured"));
          this._root = new AlwaysOnSampler();
        }
        this._remoteParentSampled = (_a2 = config.remoteParentSampled) !== null && _a2 !== void 0 ? _a2 : new AlwaysOnSampler();
        this._remoteParentNotSampled = (_b = config.remoteParentNotSampled) !== null && _b !== void 0 ? _b : new AlwaysOffSampler();
        this._localParentSampled = (_c = config.localParentSampled) !== null && _c !== void 0 ? _c : new AlwaysOnSampler();
        this._localParentNotSampled = (_d = config.localParentNotSampled) !== null && _d !== void 0 ? _d : new AlwaysOffSampler();
      }
      ParentBasedSampler2.prototype.shouldSample = function(context2, traceId, spanName, spanKind, attributes, links) {
        var parentContext = trace.getSpanContext(context2);
        if (!parentContext || !isSpanContextValid(parentContext)) {
          return this._root.shouldSample(context2, traceId, spanName, spanKind, attributes, links);
        }
        if (parentContext.isRemote) {
          if (parentContext.traceFlags & TraceFlags.SAMPLED) {
            return this._remoteParentSampled.shouldSample(context2, traceId, spanName, spanKind, attributes, links);
          }
          return this._remoteParentNotSampled.shouldSample(context2, traceId, spanName, spanKind, attributes, links);
        }
        if (parentContext.traceFlags & TraceFlags.SAMPLED) {
          return this._localParentSampled.shouldSample(context2, traceId, spanName, spanKind, attributes, links);
        }
        return this._localParentNotSampled.shouldSample(context2, traceId, spanName, spanKind, attributes, links);
      };
      ParentBasedSampler2.prototype.toString = function() {
        return "ParentBased{root=" + this._root.toString() + ", remoteParentSampled=" + this._remoteParentSampled.toString() + ", remoteParentNotSampled=" + this._remoteParentNotSampled.toString() + ", localParentSampled=" + this._localParentSampled.toString() + ", localParentNotSampled=" + this._localParentNotSampled.toString() + "}";
      };
      return ParentBasedSampler2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/sampler/TraceIdRatioBasedSampler.js
  var TraceIdRatioBasedSampler = (
    /** @class */
    function() {
      function TraceIdRatioBasedSampler2(_ratio) {
        if (_ratio === void 0) {
          _ratio = 0;
        }
        this._ratio = _ratio;
        this._ratio = this._normalize(_ratio);
        this._upperBound = Math.floor(this._ratio * 4294967295);
      }
      TraceIdRatioBasedSampler2.prototype.shouldSample = function(context2, traceId) {
        return {
          decision: isValidTraceId(traceId) && this._accumulate(traceId) < this._upperBound ? SamplingDecision2.RECORD_AND_SAMPLED : SamplingDecision2.NOT_RECORD
        };
      };
      TraceIdRatioBasedSampler2.prototype.toString = function() {
        return "TraceIdRatioBased{" + this._ratio + "}";
      };
      TraceIdRatioBasedSampler2.prototype._normalize = function(ratio) {
        if (typeof ratio !== "number" || isNaN(ratio))
          return 0;
        return ratio >= 1 ? 1 : ratio <= 0 ? 0 : ratio;
      };
      TraceIdRatioBasedSampler2.prototype._accumulate = function(traceId) {
        var accumulation = 0;
        for (var i2 = 0; i2 < traceId.length / 8; i2++) {
          var pos = i2 * 8;
          var part = parseInt(traceId.slice(pos, pos + 8), 16);
          accumulation = (accumulation ^ part) >>> 0;
        }
        return accumulation;
      };
      return TraceIdRatioBasedSampler2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/config.js
  var env = getEnv();
  var FALLBACK_OTEL_TRACES_SAMPLER = TracesSamplerValues.AlwaysOn;
  var DEFAULT_RATIO = 1;
  function loadDefaultConfig() {
    return {
      sampler: buildSamplerFromEnv(env),
      forceFlushTimeoutMillis: 3e4,
      generalLimits: {
        attributeValueLengthLimit: getEnv().OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT,
        attributeCountLimit: getEnv().OTEL_ATTRIBUTE_COUNT_LIMIT
      },
      spanLimits: {
        attributeValueLengthLimit: getEnv().OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT,
        attributeCountLimit: getEnv().OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT,
        linkCountLimit: getEnv().OTEL_SPAN_LINK_COUNT_LIMIT,
        eventCountLimit: getEnv().OTEL_SPAN_EVENT_COUNT_LIMIT,
        attributePerEventCountLimit: getEnv().OTEL_SPAN_ATTRIBUTE_PER_EVENT_COUNT_LIMIT,
        attributePerLinkCountLimit: getEnv().OTEL_SPAN_ATTRIBUTE_PER_LINK_COUNT_LIMIT
      }
    };
  }
  function buildSamplerFromEnv(environment) {
    if (environment === void 0) {
      environment = getEnv();
    }
    switch (environment.OTEL_TRACES_SAMPLER) {
      case TracesSamplerValues.AlwaysOn:
        return new AlwaysOnSampler();
      case TracesSamplerValues.AlwaysOff:
        return new AlwaysOffSampler();
      case TracesSamplerValues.ParentBasedAlwaysOn:
        return new ParentBasedSampler({
          root: new AlwaysOnSampler()
        });
      case TracesSamplerValues.ParentBasedAlwaysOff:
        return new ParentBasedSampler({
          root: new AlwaysOffSampler()
        });
      case TracesSamplerValues.TraceIdRatio:
        return new TraceIdRatioBasedSampler(getSamplerProbabilityFromEnv(environment));
      case TracesSamplerValues.ParentBasedTraceIdRatio:
        return new ParentBasedSampler({
          root: new TraceIdRatioBasedSampler(getSamplerProbabilityFromEnv(environment))
        });
      default:
        diag2.error('OTEL_TRACES_SAMPLER value "' + environment.OTEL_TRACES_SAMPLER + " invalid, defaulting to " + FALLBACK_OTEL_TRACES_SAMPLER + '".');
        return new AlwaysOnSampler();
    }
  }
  function getSamplerProbabilityFromEnv(environment) {
    if (environment.OTEL_TRACES_SAMPLER_ARG === void 0 || environment.OTEL_TRACES_SAMPLER_ARG === "") {
      diag2.error("OTEL_TRACES_SAMPLER_ARG is blank, defaulting to " + DEFAULT_RATIO + ".");
      return DEFAULT_RATIO;
    }
    var probability = Number(environment.OTEL_TRACES_SAMPLER_ARG);
    if (isNaN(probability)) {
      diag2.error("OTEL_TRACES_SAMPLER_ARG=" + environment.OTEL_TRACES_SAMPLER_ARG + " was given, but it is invalid, defaulting to " + DEFAULT_RATIO + ".");
      return DEFAULT_RATIO;
    }
    if (probability < 0 || probability > 1) {
      diag2.error("OTEL_TRACES_SAMPLER_ARG=" + environment.OTEL_TRACES_SAMPLER_ARG + " was given, but it is out of range ([0..1]), defaulting to " + DEFAULT_RATIO + ".");
      return DEFAULT_RATIO;
    }
    return probability;
  }

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/utility.js
  function mergeConfig(userConfig) {
    var perInstanceDefaults = {
      sampler: buildSamplerFromEnv()
    };
    var DEFAULT_CONFIG = loadDefaultConfig();
    var target = Object.assign({}, DEFAULT_CONFIG, perInstanceDefaults, userConfig);
    target.generalLimits = Object.assign({}, DEFAULT_CONFIG.generalLimits, userConfig.generalLimits || {});
    target.spanLimits = Object.assign({}, DEFAULT_CONFIG.spanLimits, userConfig.spanLimits || {});
    return target;
  }
  function reconfigureLimits(userConfig) {
    var _a2, _b, _c, _d, _e2, _f, _g, _h, _j, _k, _l, _m;
    var spanLimits = Object.assign({}, userConfig.spanLimits);
    var parsedEnvConfig = getEnvWithoutDefaults();
    spanLimits.attributeCountLimit = (_f = (_e2 = (_d = (_b = (_a2 = userConfig.spanLimits) === null || _a2 === void 0 ? void 0 : _a2.attributeCountLimit) !== null && _b !== void 0 ? _b : (_c = userConfig.generalLimits) === null || _c === void 0 ? void 0 : _c.attributeCountLimit) !== null && _d !== void 0 ? _d : parsedEnvConfig.OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT) !== null && _e2 !== void 0 ? _e2 : parsedEnvConfig.OTEL_ATTRIBUTE_COUNT_LIMIT) !== null && _f !== void 0 ? _f : DEFAULT_ATTRIBUTE_COUNT_LIMIT;
    spanLimits.attributeValueLengthLimit = (_m = (_l = (_k = (_h = (_g = userConfig.spanLimits) === null || _g === void 0 ? void 0 : _g.attributeValueLengthLimit) !== null && _h !== void 0 ? _h : (_j = userConfig.generalLimits) === null || _j === void 0 ? void 0 : _j.attributeValueLengthLimit) !== null && _k !== void 0 ? _k : parsedEnvConfig.OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT) !== null && _l !== void 0 ? _l : parsedEnvConfig.OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT) !== null && _m !== void 0 ? _m : DEFAULT_ATTRIBUTE_VALUE_LENGTH_LIMIT;
    return Object.assign({}, userConfig, { spanLimits });
  }

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/export/BatchSpanProcessorBase.js
  var BatchSpanProcessorBase = (
    /** @class */
    function() {
      function BatchSpanProcessorBase2(_exporter, config) {
        this._exporter = _exporter;
        this._finishedSpans = [];
        this._droppedSpansCount = 0;
        var env2 = getEnv();
        this._maxExportBatchSize = typeof (config === null || config === void 0 ? void 0 : config.maxExportBatchSize) === "number" ? config.maxExportBatchSize : env2.OTEL_BSP_MAX_EXPORT_BATCH_SIZE;
        this._maxQueueSize = typeof (config === null || config === void 0 ? void 0 : config.maxQueueSize) === "number" ? config.maxQueueSize : env2.OTEL_BSP_MAX_QUEUE_SIZE;
        this._scheduledDelayMillis = typeof (config === null || config === void 0 ? void 0 : config.scheduledDelayMillis) === "number" ? config.scheduledDelayMillis : env2.OTEL_BSP_SCHEDULE_DELAY;
        this._exportTimeoutMillis = typeof (config === null || config === void 0 ? void 0 : config.exportTimeoutMillis) === "number" ? config.exportTimeoutMillis : env2.OTEL_BSP_EXPORT_TIMEOUT;
        this._shutdownOnce = new BindOnceFuture(this._shutdown, this);
        if (this._maxExportBatchSize > this._maxQueueSize) {
          diag2.warn("BatchSpanProcessor: maxExportBatchSize must be smaller or equal to maxQueueSize, setting maxExportBatchSize to match maxQueueSize");
          this._maxExportBatchSize = this._maxQueueSize;
        }
      }
      BatchSpanProcessorBase2.prototype.forceFlush = function() {
        if (this._shutdownOnce.isCalled) {
          return this._shutdownOnce.promise;
        }
        return this._flushAll();
      };
      BatchSpanProcessorBase2.prototype.onStart = function(_span, _parentContext) {
      };
      BatchSpanProcessorBase2.prototype.onEnd = function(span) {
        if (this._shutdownOnce.isCalled) {
          return;
        }
        if ((span.spanContext().traceFlags & TraceFlags.SAMPLED) === 0) {
          return;
        }
        this._addToBuffer(span);
      };
      BatchSpanProcessorBase2.prototype.shutdown = function() {
        return this._shutdownOnce.call();
      };
      BatchSpanProcessorBase2.prototype._shutdown = function() {
        var _this = this;
        return Promise.resolve().then(function() {
          return _this.onShutdown();
        }).then(function() {
          return _this._flushAll();
        }).then(function() {
          return _this._exporter.shutdown();
        });
      };
      BatchSpanProcessorBase2.prototype._addToBuffer = function(span) {
        if (this._finishedSpans.length >= this._maxQueueSize) {
          if (this._droppedSpansCount === 0) {
            diag2.debug("maxQueueSize reached, dropping spans");
          }
          this._droppedSpansCount++;
          return;
        }
        if (this._droppedSpansCount > 0) {
          diag2.warn("Dropped " + this._droppedSpansCount + " spans because maxQueueSize reached");
          this._droppedSpansCount = 0;
        }
        this._finishedSpans.push(span);
        this._maybeStartTimer();
      };
      BatchSpanProcessorBase2.prototype._flushAll = function() {
        var _this = this;
        return new Promise(function(resolve, reject) {
          var promises = [];
          var count = Math.ceil(_this._finishedSpans.length / _this._maxExportBatchSize);
          for (var i2 = 0, j2 = count; i2 < j2; i2++) {
            promises.push(_this._flushOneBatch());
          }
          Promise.all(promises).then(function() {
            resolve();
          }).catch(reject);
        });
      };
      BatchSpanProcessorBase2.prototype._flushOneBatch = function() {
        var _this = this;
        this._clearTimer();
        if (this._finishedSpans.length === 0) {
          return Promise.resolve();
        }
        return new Promise(function(resolve, reject) {
          var timer = setTimeout(function() {
            reject(new Error("Timeout"));
          }, _this._exportTimeoutMillis);
          context.with(suppressTracing(context.active()), function() {
            var spans = _this._finishedSpans.splice(0, _this._maxExportBatchSize);
            var doExport = function() {
              return _this._exporter.export(spans, function(result) {
                var _a2;
                clearTimeout(timer);
                if (result.code === ExportResultCode.SUCCESS) {
                  resolve();
                } else {
                  reject((_a2 = result.error) !== null && _a2 !== void 0 ? _a2 : new Error("BatchSpanProcessor: span export failed"));
                }
              });
            };
            var pendingResources = spans.map(function(span) {
              return span.resource;
            }).filter(function(resource) {
              return resource.asyncAttributesPending;
            });
            if (pendingResources.length === 0) {
              doExport();
            } else {
              Promise.all(pendingResources.map(function(resource) {
                var _a2;
                return (_a2 = resource.waitForAsyncAttributes) === null || _a2 === void 0 ? void 0 : _a2.call(resource);
              })).then(doExport, function(err) {
                globalErrorHandler(err);
                reject(err);
              });
            }
          });
        });
      };
      BatchSpanProcessorBase2.prototype._maybeStartTimer = function() {
        var _this = this;
        if (this._timer !== void 0)
          return;
        this._timer = setTimeout(function() {
          _this._flushOneBatch().then(function() {
            if (_this._finishedSpans.length > 0) {
              _this._clearTimer();
              _this._maybeStartTimer();
            }
          }).catch(function(e2) {
            globalErrorHandler(e2);
          });
        }, this._scheduledDelayMillis);
        unrefTimer(this._timer);
      };
      BatchSpanProcessorBase2.prototype._clearTimer = function() {
        if (this._timer !== void 0) {
          clearTimeout(this._timer);
          this._timer = void 0;
        }
      };
      return BatchSpanProcessorBase2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/platform/browser/export/BatchSpanProcessor.js
  var __extends2 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var BatchSpanProcessor = (
    /** @class */
    function(_super) {
      __extends2(BatchSpanProcessor2, _super);
      function BatchSpanProcessor2(_exporter, config) {
        var _this = _super.call(this, _exporter, config) || this;
        _this.onInit(config);
        return _this;
      }
      BatchSpanProcessor2.prototype.onInit = function(config) {
        var _this = this;
        if ((config === null || config === void 0 ? void 0 : config.disableAutoFlushOnDocumentHide) !== true && typeof document !== "undefined") {
          this._visibilityChangeListener = function() {
            if (document.visibilityState === "hidden") {
              void _this.forceFlush();
            }
          };
          this._pageHideListener = function() {
            void _this.forceFlush();
          };
          document.addEventListener("visibilitychange", this._visibilityChangeListener);
          document.addEventListener("pagehide", this._pageHideListener);
        }
      };
      BatchSpanProcessor2.prototype.onShutdown = function() {
        if (typeof document !== "undefined") {
          if (this._visibilityChangeListener) {
            document.removeEventListener("visibilitychange", this._visibilityChangeListener);
          }
          if (this._pageHideListener) {
            document.removeEventListener("pagehide", this._pageHideListener);
          }
        }
      };
      return BatchSpanProcessor2;
    }(BatchSpanProcessorBase)
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/platform/browser/RandomIdGenerator.js
  var SPAN_ID_BYTES = 8;
  var TRACE_ID_BYTES = 16;
  var RandomIdGenerator = (
    /** @class */
    function() {
      function RandomIdGenerator2() {
        this.generateTraceId = getIdGenerator(TRACE_ID_BYTES);
        this.generateSpanId = getIdGenerator(SPAN_ID_BYTES);
      }
      return RandomIdGenerator2;
    }()
  );
  var SHARED_CHAR_CODES_ARRAY = Array(32);
  function getIdGenerator(bytes) {
    return function generateId() {
      for (var i2 = 0; i2 < bytes * 2; i2++) {
        SHARED_CHAR_CODES_ARRAY[i2] = Math.floor(Math.random() * 16) + 48;
        if (SHARED_CHAR_CODES_ARRAY[i2] >= 58) {
          SHARED_CHAR_CODES_ARRAY[i2] += 39;
        }
      }
      return String.fromCharCode.apply(null, SHARED_CHAR_CODES_ARRAY.slice(0, bytes * 2));
    };
  }

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/Tracer.js
  var Tracer = (
    /** @class */
    function() {
      function Tracer2(instrumentationLibrary, config, _tracerProvider) {
        this._tracerProvider = _tracerProvider;
        var localConfig = mergeConfig(config);
        this._sampler = localConfig.sampler;
        this._generalLimits = localConfig.generalLimits;
        this._spanLimits = localConfig.spanLimits;
        this._idGenerator = config.idGenerator || new RandomIdGenerator();
        this.resource = _tracerProvider.resource;
        this.instrumentationLibrary = instrumentationLibrary;
      }
      Tracer2.prototype.startSpan = function(name, options, context2) {
        var _a2, _b, _c;
        if (options === void 0) {
          options = {};
        }
        if (context2 === void 0) {
          context2 = context.active();
        }
        if (options.root) {
          context2 = trace.deleteSpan(context2);
        }
        var parentSpan = trace.getSpan(context2);
        if (isTracingSuppressed(context2)) {
          diag2.debug("Instrumentation suppressed, returning Noop Span");
          var nonRecordingSpan = trace.wrapSpanContext(INVALID_SPAN_CONTEXT);
          return nonRecordingSpan;
        }
        var parentSpanContext = parentSpan === null || parentSpan === void 0 ? void 0 : parentSpan.spanContext();
        var spanId = this._idGenerator.generateSpanId();
        var traceId;
        var traceState;
        var parentSpanId;
        if (!parentSpanContext || !trace.isSpanContextValid(parentSpanContext)) {
          traceId = this._idGenerator.generateTraceId();
        } else {
          traceId = parentSpanContext.traceId;
          traceState = parentSpanContext.traceState;
          parentSpanId = parentSpanContext.spanId;
        }
        var spanKind = (_a2 = options.kind) !== null && _a2 !== void 0 ? _a2 : SpanKind.INTERNAL;
        var links = ((_b = options.links) !== null && _b !== void 0 ? _b : []).map(function(link) {
          return {
            context: link.context,
            attributes: sanitizeAttributes(link.attributes)
          };
        });
        var attributes = sanitizeAttributes(options.attributes);
        var samplingResult = this._sampler.shouldSample(context2, traceId, name, spanKind, attributes, links);
        traceState = (_c = samplingResult.traceState) !== null && _c !== void 0 ? _c : traceState;
        var traceFlags = samplingResult.decision === SamplingDecision.RECORD_AND_SAMPLED ? TraceFlags.SAMPLED : TraceFlags.NONE;
        var spanContext = { traceId, spanId, traceFlags, traceState };
        if (samplingResult.decision === SamplingDecision.NOT_RECORD) {
          diag2.debug("Recording is off, propagating context in a non-recording span");
          var nonRecordingSpan = trace.wrapSpanContext(spanContext);
          return nonRecordingSpan;
        }
        var span = new Span(this, context2, name, spanContext, spanKind, parentSpanId, links, options.startTime);
        var initAttributes = sanitizeAttributes(Object.assign(attributes, samplingResult.attributes));
        span.setAttributes(initAttributes);
        return span;
      };
      Tracer2.prototype.startActiveSpan = function(name, arg2, arg3, arg4) {
        var opts;
        var ctx;
        var fn;
        if (arguments.length < 2) {
          return;
        } else if (arguments.length === 2) {
          fn = arg2;
        } else if (arguments.length === 3) {
          opts = arg2;
          fn = arg3;
        } else {
          opts = arg2;
          ctx = arg3;
          fn = arg4;
        }
        var parentContext = ctx !== null && ctx !== void 0 ? ctx : context.active();
        var span = this.startSpan(name, opts, parentContext);
        var contextWithSpanSet = trace.setSpan(parentContext, span);
        return context.with(contextWithSpanSet, fn, void 0, span);
      };
      Tracer2.prototype.getGeneralLimits = function() {
        return this._generalLimits;
      };
      Tracer2.prototype.getSpanLimits = function() {
        return this._spanLimits;
      };
      Tracer2.prototype.getActiveSpanProcessor = function() {
        return this._tracerProvider.getActiveSpanProcessor();
      };
      return Tracer2;
    }()
  );

  // node_modules/@opentelemetry/resources/build/esm/platform/browser/default-service-name.js
  function defaultServiceName() {
    return "unknown_service";
  }

  // node_modules/@opentelemetry/resources/build/esm/Resource.js
  var __assign = function() {
    __assign = Object.assign || function(t2) {
      for (var s2, i2 = 1, n2 = arguments.length; i2 < n2; i2++) {
        s2 = arguments[i2];
        for (var p2 in s2)
          if (Object.prototype.hasOwnProperty.call(s2, p2))
            t2[p2] = s2[p2];
      }
      return t2;
    };
    return __assign.apply(this, arguments);
  };
  var __awaiter = function(thisArg, _arguments, P2, generator) {
    function adopt(value) {
      return value instanceof P2 ? value : new P2(function(resolve) {
        resolve(value);
      });
    }
    return new (P2 || (P2 = Promise))(function(resolve, reject) {
      function fulfilled(value) {
        try {
          step(generator.next(value));
        } catch (e2) {
          reject(e2);
        }
      }
      function rejected(value) {
        try {
          step(generator["throw"](value));
        } catch (e2) {
          reject(e2);
        }
      }
      function step(result) {
        result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected);
      }
      step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
  };
  var __generator = function(thisArg, body) {
    var _2 = { label: 0, sent: function() {
      if (t2[0] & 1)
        throw t2[1];
      return t2[1];
    }, trys: [], ops: [] }, f2, y2, t2, g2;
    return g2 = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g2[Symbol.iterator] = function() {
      return this;
    }), g2;
    function verb(n2) {
      return function(v2) {
        return step([n2, v2]);
      };
    }
    function step(op) {
      if (f2)
        throw new TypeError("Generator is already executing.");
      while (_2)
        try {
          if (f2 = 1, y2 && (t2 = op[0] & 2 ? y2["return"] : op[0] ? y2["throw"] || ((t2 = y2["return"]) && t2.call(y2), 0) : y2.next) && !(t2 = t2.call(y2, op[1])).done)
            return t2;
          if (y2 = 0, t2)
            op = [op[0] & 2, t2.value];
          switch (op[0]) {
            case 0:
            case 1:
              t2 = op;
              break;
            case 4:
              _2.label++;
              return { value: op[1], done: false };
            case 5:
              _2.label++;
              y2 = op[1];
              op = [0];
              continue;
            case 7:
              op = _2.ops.pop();
              _2.trys.pop();
              continue;
            default:
              if (!(t2 = _2.trys, t2 = t2.length > 0 && t2[t2.length - 1]) && (op[0] === 6 || op[0] === 2)) {
                _2 = 0;
                continue;
              }
              if (op[0] === 3 && (!t2 || op[1] > t2[0] && op[1] < t2[3])) {
                _2.label = op[1];
                break;
              }
              if (op[0] === 6 && _2.label < t2[1]) {
                _2.label = t2[1];
                t2 = op;
                break;
              }
              if (t2 && _2.label < t2[2]) {
                _2.label = t2[2];
                _2.ops.push(op);
                break;
              }
              if (t2[2])
                _2.ops.pop();
              _2.trys.pop();
              continue;
          }
          op = body.call(thisArg, _2);
        } catch (e2) {
          op = [6, e2];
          y2 = 0;
        } finally {
          f2 = t2 = 0;
        }
      if (op[0] & 5)
        throw op[1];
      return { value: op[0] ? op[1] : void 0, done: true };
    }
  };
  var __read10 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var Resource = (
    /** @class */
    function() {
      function Resource2(attributes, asyncAttributesPromise) {
        var _this = this;
        var _a2;
        this._attributes = attributes;
        this.asyncAttributesPending = asyncAttributesPromise != null;
        this._syncAttributes = (_a2 = this._attributes) !== null && _a2 !== void 0 ? _a2 : {};
        this._asyncAttributesPromise = asyncAttributesPromise === null || asyncAttributesPromise === void 0 ? void 0 : asyncAttributesPromise.then(function(asyncAttributes) {
          _this._attributes = Object.assign({}, _this._attributes, asyncAttributes);
          _this.asyncAttributesPending = false;
          return asyncAttributes;
        }, function(err) {
          diag2.debug("a resource's async attributes promise rejected: %s", err);
          _this.asyncAttributesPending = false;
          return {};
        });
      }
      Resource2.empty = function() {
        return Resource2.EMPTY;
      };
      Resource2.default = function() {
        var _a2;
        return new Resource2((_a2 = {}, _a2[SemanticResourceAttributes.SERVICE_NAME] = defaultServiceName(), _a2[SemanticResourceAttributes.TELEMETRY_SDK_LANGUAGE] = SDK_INFO[SemanticResourceAttributes.TELEMETRY_SDK_LANGUAGE], _a2[SemanticResourceAttributes.TELEMETRY_SDK_NAME] = SDK_INFO[SemanticResourceAttributes.TELEMETRY_SDK_NAME], _a2[SemanticResourceAttributes.TELEMETRY_SDK_VERSION] = SDK_INFO[SemanticResourceAttributes.TELEMETRY_SDK_VERSION], _a2));
      };
      Object.defineProperty(Resource2.prototype, "attributes", {
        get: function() {
          var _a2;
          if (this.asyncAttributesPending) {
            diag2.error("Accessing resource attributes before async attributes settled");
          }
          return (_a2 = this._attributes) !== null && _a2 !== void 0 ? _a2 : {};
        },
        enumerable: false,
        configurable: true
      });
      Resource2.prototype.waitForAsyncAttributes = function() {
        return __awaiter(this, void 0, void 0, function() {
          return __generator(this, function(_a2) {
            switch (_a2.label) {
              case 0:
                if (!this.asyncAttributesPending)
                  return [3, 2];
                return [4, this._asyncAttributesPromise];
              case 1:
                _a2.sent();
                _a2.label = 2;
              case 2:
                return [
                  2
                  /*return*/
                ];
            }
          });
        });
      };
      Resource2.prototype.merge = function(other) {
        var _this = this;
        var _a2;
        if (!other)
          return this;
        var mergedSyncAttributes = __assign(__assign({}, this._syncAttributes), (_a2 = other._syncAttributes) !== null && _a2 !== void 0 ? _a2 : other.attributes);
        if (!this._asyncAttributesPromise && !other._asyncAttributesPromise) {
          return new Resource2(mergedSyncAttributes);
        }
        var mergedAttributesPromise = Promise.all([
          this._asyncAttributesPromise,
          other._asyncAttributesPromise
        ]).then(function(_a3) {
          var _b;
          var _c = __read10(_a3, 2), thisAsyncAttributes = _c[0], otherAsyncAttributes = _c[1];
          return __assign(__assign(__assign(__assign({}, _this._syncAttributes), thisAsyncAttributes), (_b = other._syncAttributes) !== null && _b !== void 0 ? _b : other.attributes), otherAsyncAttributes);
        });
        return new Resource2(mergedSyncAttributes, mergedAttributesPromise);
      };
      Resource2.EMPTY = new Resource2({});
      return Resource2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/MultiSpanProcessor.js
  var __values6 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var MultiSpanProcessor = (
    /** @class */
    function() {
      function MultiSpanProcessor2(_spanProcessors) {
        this._spanProcessors = _spanProcessors;
      }
      MultiSpanProcessor2.prototype.forceFlush = function() {
        var e_1, _a2;
        var promises = [];
        try {
          for (var _b = __values6(this._spanProcessors), _c = _b.next(); !_c.done; _c = _b.next()) {
            var spanProcessor = _c.value;
            promises.push(spanProcessor.forceFlush());
          }
        } catch (e_1_1) {
          e_1 = { error: e_1_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_1)
              throw e_1.error;
          }
        }
        return new Promise(function(resolve) {
          Promise.all(promises).then(function() {
            resolve();
          }).catch(function(error) {
            globalErrorHandler(error || new Error("MultiSpanProcessor: forceFlush failed"));
            resolve();
          });
        });
      };
      MultiSpanProcessor2.prototype.onStart = function(span, context2) {
        var e_2, _a2;
        try {
          for (var _b = __values6(this._spanProcessors), _c = _b.next(); !_c.done; _c = _b.next()) {
            var spanProcessor = _c.value;
            spanProcessor.onStart(span, context2);
          }
        } catch (e_2_1) {
          e_2 = { error: e_2_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_2)
              throw e_2.error;
          }
        }
      };
      MultiSpanProcessor2.prototype.onEnd = function(span) {
        var e_3, _a2;
        try {
          for (var _b = __values6(this._spanProcessors), _c = _b.next(); !_c.done; _c = _b.next()) {
            var spanProcessor = _c.value;
            spanProcessor.onEnd(span);
          }
        } catch (e_3_1) {
          e_3 = { error: e_3_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_3)
              throw e_3.error;
          }
        }
      };
      MultiSpanProcessor2.prototype.shutdown = function() {
        var e_4, _a2;
        var promises = [];
        try {
          for (var _b = __values6(this._spanProcessors), _c = _b.next(); !_c.done; _c = _b.next()) {
            var spanProcessor = _c.value;
            promises.push(spanProcessor.shutdown());
          }
        } catch (e_4_1) {
          e_4 = { error: e_4_1 };
        } finally {
          try {
            if (_c && !_c.done && (_a2 = _b.return))
              _a2.call(_b);
          } finally {
            if (e_4)
              throw e_4.error;
          }
        }
        return new Promise(function(resolve, reject) {
          Promise.all(promises).then(function() {
            resolve();
          }, reject);
        });
      };
      return MultiSpanProcessor2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/export/NoopSpanProcessor.js
  var NoopSpanProcessor = (
    /** @class */
    function() {
      function NoopSpanProcessor2() {
      }
      NoopSpanProcessor2.prototype.onStart = function(_span, _context) {
      };
      NoopSpanProcessor2.prototype.onEnd = function(_span) {
      };
      NoopSpanProcessor2.prototype.shutdown = function() {
        return Promise.resolve();
      };
      NoopSpanProcessor2.prototype.forceFlush = function() {
        return Promise.resolve();
      };
      return NoopSpanProcessor2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/BasicTracerProvider.js
  var ForceFlushState;
  (function(ForceFlushState2) {
    ForceFlushState2[ForceFlushState2["resolved"] = 0] = "resolved";
    ForceFlushState2[ForceFlushState2["timeout"] = 1] = "timeout";
    ForceFlushState2[ForceFlushState2["error"] = 2] = "error";
    ForceFlushState2[ForceFlushState2["unresolved"] = 3] = "unresolved";
  })(ForceFlushState || (ForceFlushState = {}));
  var BasicTracerProvider = (
    /** @class */
    function() {
      function BasicTracerProvider2(config) {
        if (config === void 0) {
          config = {};
        }
        var _a2;
        this._registeredSpanProcessors = [];
        this._tracers = /* @__PURE__ */ new Map();
        var mergedConfig = merge({}, loadDefaultConfig(), reconfigureLimits(config));
        this.resource = (_a2 = mergedConfig.resource) !== null && _a2 !== void 0 ? _a2 : Resource.empty();
        this.resource = Resource.default().merge(this.resource);
        this._config = Object.assign({}, mergedConfig, {
          resource: this.resource
        });
        var defaultExporter = this._buildExporterFromEnv();
        if (defaultExporter !== void 0) {
          var batchProcessor = new BatchSpanProcessor(defaultExporter);
          this.activeSpanProcessor = batchProcessor;
        } else {
          this.activeSpanProcessor = new NoopSpanProcessor();
        }
      }
      BasicTracerProvider2.prototype.getTracer = function(name, version, options) {
        var key = name + "@" + (version || "") + ":" + ((options === null || options === void 0 ? void 0 : options.schemaUrl) || "");
        if (!this._tracers.has(key)) {
          this._tracers.set(key, new Tracer({ name, version, schemaUrl: options === null || options === void 0 ? void 0 : options.schemaUrl }, this._config, this));
        }
        return this._tracers.get(key);
      };
      BasicTracerProvider2.prototype.addSpanProcessor = function(spanProcessor) {
        if (this._registeredSpanProcessors.length === 0) {
          this.activeSpanProcessor.shutdown().catch(function(err) {
            return diag2.error("Error while trying to shutdown current span processor", err);
          });
        }
        this._registeredSpanProcessors.push(spanProcessor);
        this.activeSpanProcessor = new MultiSpanProcessor(this._registeredSpanProcessors);
      };
      BasicTracerProvider2.prototype.getActiveSpanProcessor = function() {
        return this.activeSpanProcessor;
      };
      BasicTracerProvider2.prototype.register = function(config) {
        if (config === void 0) {
          config = {};
        }
        trace.setGlobalTracerProvider(this);
        if (config.propagator === void 0) {
          config.propagator = this._buildPropagatorFromEnv();
        }
        if (config.contextManager) {
          context.setGlobalContextManager(config.contextManager);
        }
        if (config.propagator) {
          propagation.setGlobalPropagator(config.propagator);
        }
      };
      BasicTracerProvider2.prototype.forceFlush = function() {
        var timeout = this._config.forceFlushTimeoutMillis;
        var promises = this._registeredSpanProcessors.map(function(spanProcessor) {
          return new Promise(function(resolve) {
            var state;
            var timeoutInterval = setTimeout(function() {
              resolve(new Error("Span processor did not completed within timeout period of " + timeout + " ms"));
              state = ForceFlushState.timeout;
            }, timeout);
            spanProcessor.forceFlush().then(function() {
              clearTimeout(timeoutInterval);
              if (state !== ForceFlushState.timeout) {
                state = ForceFlushState.resolved;
                resolve(state);
              }
            }).catch(function(error) {
              clearTimeout(timeoutInterval);
              state = ForceFlushState.error;
              resolve(error);
            });
          });
        });
        return new Promise(function(resolve, reject) {
          Promise.all(promises).then(function(results) {
            var errors = results.filter(function(result) {
              return result !== ForceFlushState.resolved;
            });
            if (errors.length > 0) {
              reject(errors);
            } else {
              resolve();
            }
          }).catch(function(error) {
            return reject([error]);
          });
        });
      };
      BasicTracerProvider2.prototype.shutdown = function() {
        return this.activeSpanProcessor.shutdown();
      };
      BasicTracerProvider2.prototype._getPropagator = function(name) {
        var _a2;
        return (_a2 = this.constructor._registeredPropagators.get(name)) === null || _a2 === void 0 ? void 0 : _a2();
      };
      BasicTracerProvider2.prototype._getSpanExporter = function(name) {
        var _a2;
        return (_a2 = this.constructor._registeredExporters.get(name)) === null || _a2 === void 0 ? void 0 : _a2();
      };
      BasicTracerProvider2.prototype._buildPropagatorFromEnv = function() {
        var _this = this;
        var uniquePropagatorNames = Array.from(new Set(getEnv().OTEL_PROPAGATORS));
        var propagators = uniquePropagatorNames.map(function(name) {
          var propagator = _this._getPropagator(name);
          if (!propagator) {
            diag2.warn('Propagator "' + name + '" requested through environment variable is unavailable.');
          }
          return propagator;
        });
        var validPropagators = propagators.reduce(function(list, item) {
          if (item) {
            list.push(item);
          }
          return list;
        }, []);
        if (validPropagators.length === 0) {
          return;
        } else if (uniquePropagatorNames.length === 1) {
          return validPropagators[0];
        } else {
          return new CompositePropagator({
            propagators: validPropagators
          });
        }
      };
      BasicTracerProvider2.prototype._buildExporterFromEnv = function() {
        var exporterName = getEnv().OTEL_TRACES_EXPORTER;
        if (exporterName === "none" || exporterName === "")
          return;
        var exporter = this._getSpanExporter(exporterName);
        if (!exporter) {
          diag2.error('Exporter "' + exporterName + '" requested through environment variable is unavailable.');
        }
        return exporter;
      };
      BasicTracerProvider2._registeredPropagators = /* @__PURE__ */ new Map([
        ["tracecontext", function() {
          return new W3CTraceContextPropagator();
        }],
        ["baggage", function() {
          return new W3CBaggagePropagator();
        }]
      ]);
      BasicTracerProvider2._registeredExporters = /* @__PURE__ */ new Map();
      return BasicTracerProvider2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/export/ConsoleSpanExporter.js
  var __values7 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var ConsoleSpanExporter = (
    /** @class */
    function() {
      function ConsoleSpanExporter2() {
      }
      ConsoleSpanExporter2.prototype.export = function(spans, resultCallback) {
        return this._sendSpans(spans, resultCallback);
      };
      ConsoleSpanExporter2.prototype.shutdown = function() {
        this._sendSpans([]);
        return Promise.resolve();
      };
      ConsoleSpanExporter2.prototype._exportInfo = function(span) {
        var _a2;
        return {
          traceId: span.spanContext().traceId,
          parentId: span.parentSpanId,
          traceState: (_a2 = span.spanContext().traceState) === null || _a2 === void 0 ? void 0 : _a2.serialize(),
          name: span.name,
          id: span.spanContext().spanId,
          kind: span.kind,
          timestamp: hrTimeToMicroseconds(span.startTime),
          duration: hrTimeToMicroseconds(span.duration),
          attributes: span.attributes,
          status: span.status,
          events: span.events,
          links: span.links
        };
      };
      ConsoleSpanExporter2.prototype._sendSpans = function(spans, done) {
        var e_1, _a2;
        try {
          for (var spans_1 = __values7(spans), spans_1_1 = spans_1.next(); !spans_1_1.done; spans_1_1 = spans_1.next()) {
            var span = spans_1_1.value;
            console.dir(this._exportInfo(span), { depth: 3 });
          }
        } catch (e_1_1) {
          e_1 = { error: e_1_1 };
        } finally {
          try {
            if (spans_1_1 && !spans_1_1.done && (_a2 = spans_1.return))
              _a2.call(spans_1);
          } finally {
            if (e_1)
              throw e_1.error;
          }
        }
        if (done) {
          return done({ code: ExportResultCode.SUCCESS });
        }
      };
      return ConsoleSpanExporter2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-base/build/esm/export/SimpleSpanProcessor.js
  var __awaiter2 = function(thisArg, _arguments, P2, generator) {
    function adopt(value) {
      return value instanceof P2 ? value : new P2(function(resolve) {
        resolve(value);
      });
    }
    return new (P2 || (P2 = Promise))(function(resolve, reject) {
      function fulfilled(value) {
        try {
          step(generator.next(value));
        } catch (e2) {
          reject(e2);
        }
      }
      function rejected(value) {
        try {
          step(generator["throw"](value));
        } catch (e2) {
          reject(e2);
        }
      }
      function step(result) {
        result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected);
      }
      step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
  };
  var __generator2 = function(thisArg, body) {
    var _2 = { label: 0, sent: function() {
      if (t2[0] & 1)
        throw t2[1];
      return t2[1];
    }, trys: [], ops: [] }, f2, y2, t2, g2;
    return g2 = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g2[Symbol.iterator] = function() {
      return this;
    }), g2;
    function verb(n2) {
      return function(v2) {
        return step([n2, v2]);
      };
    }
    function step(op) {
      if (f2)
        throw new TypeError("Generator is already executing.");
      while (_2)
        try {
          if (f2 = 1, y2 && (t2 = op[0] & 2 ? y2["return"] : op[0] ? y2["throw"] || ((t2 = y2["return"]) && t2.call(y2), 0) : y2.next) && !(t2 = t2.call(y2, op[1])).done)
            return t2;
          if (y2 = 0, t2)
            op = [op[0] & 2, t2.value];
          switch (op[0]) {
            case 0:
            case 1:
              t2 = op;
              break;
            case 4:
              _2.label++;
              return { value: op[1], done: false };
            case 5:
              _2.label++;
              y2 = op[1];
              op = [0];
              continue;
            case 7:
              op = _2.ops.pop();
              _2.trys.pop();
              continue;
            default:
              if (!(t2 = _2.trys, t2 = t2.length > 0 && t2[t2.length - 1]) && (op[0] === 6 || op[0] === 2)) {
                _2 = 0;
                continue;
              }
              if (op[0] === 3 && (!t2 || op[1] > t2[0] && op[1] < t2[3])) {
                _2.label = op[1];
                break;
              }
              if (op[0] === 6 && _2.label < t2[1]) {
                _2.label = t2[1];
                t2 = op;
                break;
              }
              if (t2 && _2.label < t2[2]) {
                _2.label = t2[2];
                _2.ops.push(op);
                break;
              }
              if (t2[2])
                _2.ops.pop();
              _2.trys.pop();
              continue;
          }
          op = body.call(thisArg, _2);
        } catch (e2) {
          op = [6, e2];
          y2 = 0;
        } finally {
          f2 = t2 = 0;
        }
      if (op[0] & 5)
        throw op[1];
      return { value: op[0] ? op[1] : void 0, done: true };
    }
  };
  var SimpleSpanProcessor = (
    /** @class */
    function() {
      function SimpleSpanProcessor2(_exporter) {
        this._exporter = _exporter;
        this._shutdownOnce = new BindOnceFuture(this._shutdown, this);
        this._unresolvedExports = /* @__PURE__ */ new Set();
      }
      SimpleSpanProcessor2.prototype.forceFlush = function() {
        return __awaiter2(this, void 0, void 0, function() {
          return __generator2(this, function(_a2) {
            switch (_a2.label) {
              case 0:
                return [4, Promise.all(Array.from(this._unresolvedExports))];
              case 1:
                _a2.sent();
                return [
                  2
                  /*return*/
                ];
            }
          });
        });
      };
      SimpleSpanProcessor2.prototype.onStart = function(_span, _parentContext) {
      };
      SimpleSpanProcessor2.prototype.onEnd = function(span) {
        var _this = this;
        var _a2, _b;
        if (this._shutdownOnce.isCalled) {
          return;
        }
        if ((span.spanContext().traceFlags & TraceFlags.SAMPLED) === 0) {
          return;
        }
        var doExport = function() {
          return internal._export(_this._exporter, [span]).then(function(result) {
            var _a3;
            if (result.code !== ExportResultCode.SUCCESS) {
              globalErrorHandler((_a3 = result.error) !== null && _a3 !== void 0 ? _a3 : new Error("SimpleSpanProcessor: span export failed (status " + result + ")"));
            }
          }).catch(function(error) {
            globalErrorHandler(error);
          });
        };
        if (span.resource.asyncAttributesPending) {
          var exportPromise_1 = (_b = (_a2 = span.resource).waitForAsyncAttributes) === null || _b === void 0 ? void 0 : _b.call(_a2).then(function() {
            if (exportPromise_1 != null) {
              _this._unresolvedExports.delete(exportPromise_1);
            }
            return doExport();
          }, function(err) {
            return globalErrorHandler(err);
          });
          if (exportPromise_1 != null) {
            this._unresolvedExports.add(exportPromise_1);
          }
        } else {
          void doExport();
        }
      };
      SimpleSpanProcessor2.prototype.shutdown = function() {
        return this._shutdownOnce.call();
      };
      SimpleSpanProcessor2.prototype._shutdown = function() {
        return this._exporter.shutdown();
      };
      return SimpleSpanProcessor2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-web/build/esm/StackContextManager.js
  var __read11 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var __spreadArray6 = function(to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i2 = 0, l2 = from.length, ar2; i2 < l2; i2++) {
        if (ar2 || !(i2 in from)) {
          if (!ar2)
            ar2 = Array.prototype.slice.call(from, 0, i2);
          ar2[i2] = from[i2];
        }
      }
    return to.concat(ar2 || Array.prototype.slice.call(from));
  };
  var StackContextManager = (
    /** @class */
    function() {
      function StackContextManager2() {
        this._enabled = false;
        this._currentContext = ROOT_CONTEXT;
      }
      StackContextManager2.prototype._bindFunction = function(context2, target) {
        if (context2 === void 0) {
          context2 = ROOT_CONTEXT;
        }
        var manager = this;
        var contextWrapper = function() {
          var _this = this;
          var args = [];
          for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
          }
          return manager.with(context2, function() {
            return target.apply(_this, args);
          });
        };
        Object.defineProperty(contextWrapper, "length", {
          enumerable: false,
          configurable: true,
          writable: false,
          value: target.length
        });
        return contextWrapper;
      };
      StackContextManager2.prototype.active = function() {
        return this._currentContext;
      };
      StackContextManager2.prototype.bind = function(context2, target) {
        if (context2 === void 0) {
          context2 = this.active();
        }
        if (typeof target === "function") {
          return this._bindFunction(context2, target);
        }
        return target;
      };
      StackContextManager2.prototype.disable = function() {
        this._currentContext = ROOT_CONTEXT;
        this._enabled = false;
        return this;
      };
      StackContextManager2.prototype.enable = function() {
        if (this._enabled) {
          return this;
        }
        this._enabled = true;
        this._currentContext = ROOT_CONTEXT;
        return this;
      };
      StackContextManager2.prototype.with = function(context2, fn, thisArg) {
        var args = [];
        for (var _i = 3; _i < arguments.length; _i++) {
          args[_i - 3] = arguments[_i];
        }
        var previousContext = this._currentContext;
        this._currentContext = context2 || ROOT_CONTEXT;
        try {
          return fn.call.apply(fn, __spreadArray6([thisArg], __read11(args), false));
        } finally {
          this._currentContext = previousContext;
        }
      };
      return StackContextManager2;
    }()
  );

  // node_modules/@opentelemetry/sdk-trace-web/build/esm/WebTracerProvider.js
  var __extends3 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var WebTracerProvider = (
    /** @class */
    function(_super) {
      __extends3(WebTracerProvider2, _super);
      function WebTracerProvider2(config) {
        if (config === void 0) {
          config = {};
        }
        var _this = _super.call(this, config) || this;
        if (config.contextManager) {
          throw "contextManager should be defined in register method not in constructor";
        }
        if (config.propagator) {
          throw "propagator should be defined in register method not in constructor";
        }
        return _this;
      }
      WebTracerProvider2.prototype.register = function(config) {
        if (config === void 0) {
          config = {};
        }
        if (config.contextManager === void 0) {
          config.contextManager = new StackContextManager();
        }
        if (config.contextManager) {
          config.contextManager.enable();
        }
        _super.prototype.register.call(this, config);
      };
      return WebTracerProvider2;
    }(BasicTracerProvider)
  );

  // node_modules/@opentelemetry/sdk-trace-web/build/esm/enums/PerformanceTimingNames.js
  var PerformanceTimingNames;
  (function(PerformanceTimingNames2) {
    PerformanceTimingNames2["CONNECT_END"] = "connectEnd";
    PerformanceTimingNames2["CONNECT_START"] = "connectStart";
    PerformanceTimingNames2["DECODED_BODY_SIZE"] = "decodedBodySize";
    PerformanceTimingNames2["DOM_COMPLETE"] = "domComplete";
    PerformanceTimingNames2["DOM_CONTENT_LOADED_EVENT_END"] = "domContentLoadedEventEnd";
    PerformanceTimingNames2["DOM_CONTENT_LOADED_EVENT_START"] = "domContentLoadedEventStart";
    PerformanceTimingNames2["DOM_INTERACTIVE"] = "domInteractive";
    PerformanceTimingNames2["DOMAIN_LOOKUP_END"] = "domainLookupEnd";
    PerformanceTimingNames2["DOMAIN_LOOKUP_START"] = "domainLookupStart";
    PerformanceTimingNames2["ENCODED_BODY_SIZE"] = "encodedBodySize";
    PerformanceTimingNames2["FETCH_START"] = "fetchStart";
    PerformanceTimingNames2["LOAD_EVENT_END"] = "loadEventEnd";
    PerformanceTimingNames2["LOAD_EVENT_START"] = "loadEventStart";
    PerformanceTimingNames2["NAVIGATION_START"] = "navigationStart";
    PerformanceTimingNames2["REDIRECT_END"] = "redirectEnd";
    PerformanceTimingNames2["REDIRECT_START"] = "redirectStart";
    PerformanceTimingNames2["REQUEST_START"] = "requestStart";
    PerformanceTimingNames2["RESPONSE_END"] = "responseEnd";
    PerformanceTimingNames2["RESPONSE_START"] = "responseStart";
    PerformanceTimingNames2["SECURE_CONNECTION_START"] = "secureConnectionStart";
    PerformanceTimingNames2["UNLOAD_EVENT_END"] = "unloadEventEnd";
    PerformanceTimingNames2["UNLOAD_EVENT_START"] = "unloadEventStart";
  })(PerformanceTimingNames || (PerformanceTimingNames = {}));

  // node_modules/@opentelemetry/sdk-trace-web/build/esm/utils.js
  var urlNormalizingAnchor;
  function getUrlNormalizingAnchor() {
    if (!urlNormalizingAnchor) {
      urlNormalizingAnchor = document.createElement("a");
    }
    return urlNormalizingAnchor;
  }
  function hasKey(obj, key) {
    return key in obj;
  }
  function addSpanNetworkEvent(span, performanceName, entries) {
    if (hasKey(entries, performanceName) && typeof entries[performanceName] === "number") {
      span.addEvent(performanceName, entries[performanceName]);
      return span;
    }
    return void 0;
  }
  function addSpanNetworkEvents(span, resource) {
    addSpanNetworkEvent(span, PerformanceTimingNames.FETCH_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.DOMAIN_LOOKUP_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.DOMAIN_LOOKUP_END, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.CONNECT_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.SECURE_CONNECTION_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.CONNECT_END, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.REQUEST_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.RESPONSE_START, resource);
    addSpanNetworkEvent(span, PerformanceTimingNames.RESPONSE_END, resource);
    var encodedLength = resource[PerformanceTimingNames.ENCODED_BODY_SIZE];
    if (encodedLength !== void 0) {
      span.setAttribute(SemanticAttributes.HTTP_RESPONSE_CONTENT_LENGTH, encodedLength);
    }
    var decodedLength = resource[PerformanceTimingNames.DECODED_BODY_SIZE];
    if (decodedLength !== void 0 && encodedLength !== decodedLength) {
      span.setAttribute(SemanticAttributes.HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED, decodedLength);
    }
  }
  function sortResources(filteredResources) {
    return filteredResources.slice().sort(function(a2, b2) {
      var valueA = a2[PerformanceTimingNames.FETCH_START];
      var valueB = b2[PerformanceTimingNames.FETCH_START];
      if (valueA > valueB) {
        return 1;
      } else if (valueA < valueB) {
        return -1;
      }
      return 0;
    });
  }
  function getResource(spanUrl, startTimeHR, endTimeHR, resources, ignoredResources, initiatorType) {
    if (ignoredResources === void 0) {
      ignoredResources = /* @__PURE__ */ new WeakSet();
    }
    var parsedSpanUrl = parseUrl(spanUrl);
    spanUrl = parsedSpanUrl.toString();
    var filteredResources = filterResourcesForSpan(spanUrl, startTimeHR, endTimeHR, resources, ignoredResources, initiatorType);
    if (filteredResources.length === 0) {
      return {
        mainRequest: void 0
      };
    }
    if (filteredResources.length === 1) {
      return {
        mainRequest: filteredResources[0]
      };
    }
    var sorted = sortResources(filteredResources);
    if (parsedSpanUrl.origin !== location.origin && sorted.length > 1) {
      var corsPreFlightRequest = sorted[0];
      var mainRequest = findMainRequest(sorted, corsPreFlightRequest[PerformanceTimingNames.RESPONSE_END], endTimeHR);
      var responseEnd = corsPreFlightRequest[PerformanceTimingNames.RESPONSE_END];
      var fetchStart = mainRequest[PerformanceTimingNames.FETCH_START];
      if (fetchStart < responseEnd) {
        mainRequest = corsPreFlightRequest;
        corsPreFlightRequest = void 0;
      }
      return {
        corsPreFlightRequest,
        mainRequest
      };
    } else {
      return {
        mainRequest: filteredResources[0]
      };
    }
  }
  function findMainRequest(resources, corsPreFlightRequestEndTime, spanEndTimeHR) {
    var spanEndTime = hrTimeToNanoseconds(spanEndTimeHR);
    var minTime = hrTimeToNanoseconds(timeInputToHrTime(corsPreFlightRequestEndTime));
    var mainRequest = resources[1];
    var bestGap;
    var length = resources.length;
    for (var i2 = 1; i2 < length; i2++) {
      var resource = resources[i2];
      var resourceStartTime = hrTimeToNanoseconds(timeInputToHrTime(resource[PerformanceTimingNames.FETCH_START]));
      var resourceEndTime = hrTimeToNanoseconds(timeInputToHrTime(resource[PerformanceTimingNames.RESPONSE_END]));
      var currentGap = spanEndTime - resourceEndTime;
      if (resourceStartTime >= minTime && (!bestGap || currentGap < bestGap)) {
        bestGap = currentGap;
        mainRequest = resource;
      }
    }
    return mainRequest;
  }
  function filterResourcesForSpan(spanUrl, startTimeHR, endTimeHR, resources, ignoredResources, initiatorType) {
    var startTime = hrTimeToNanoseconds(startTimeHR);
    var endTime = hrTimeToNanoseconds(endTimeHR);
    var filteredResources = resources.filter(function(resource) {
      var resourceStartTime = hrTimeToNanoseconds(timeInputToHrTime(resource[PerformanceTimingNames.FETCH_START]));
      var resourceEndTime = hrTimeToNanoseconds(timeInputToHrTime(resource[PerformanceTimingNames.RESPONSE_END]));
      return resource.initiatorType.toLowerCase() === (initiatorType || "xmlhttprequest") && resource.name === spanUrl && resourceStartTime >= startTime && resourceEndTime <= endTime;
    });
    if (filteredResources.length > 0) {
      filteredResources = filteredResources.filter(function(resource) {
        return !ignoredResources.has(resource);
      });
    }
    return filteredResources;
  }
  function parseUrl(url) {
    if (typeof URL === "function") {
      return new URL(url, typeof document !== "undefined" ? document.baseURI : typeof location !== "undefined" ? location.href : void 0);
    }
    var element = getUrlNormalizingAnchor();
    element.href = url;
    return element;
  }
  function shouldPropagateTraceHeaders(spanUrl, propagateTraceHeaderCorsUrls) {
    var propagateTraceHeaderUrls = propagateTraceHeaderCorsUrls || [];
    if (typeof propagateTraceHeaderUrls === "string" || propagateTraceHeaderUrls instanceof RegExp) {
      propagateTraceHeaderUrls = [propagateTraceHeaderUrls];
    }
    var parsedSpanUrl = parseUrl(spanUrl);
    if (parsedSpanUrl.origin === location.origin) {
      return true;
    } else {
      return propagateTraceHeaderUrls.some(function(propagateTraceHeaderUrl) {
        return urlMatches(spanUrl, propagateTraceHeaderUrl);
      });
    }
  }

  // node_modules/@opentelemetry/instrumentation/build/esm/autoLoaderUtils.js
  function parseInstrumentationOptions(options) {
    if (options === void 0) {
      options = [];
    }
    var instrumentations = [];
    for (var i2 = 0, j2 = options.length; i2 < j2; i2++) {
      var option = options[i2];
      if (Array.isArray(option)) {
        var results = parseInstrumentationOptions(option);
        instrumentations = instrumentations.concat(results.instrumentations);
      } else if (typeof option === "function") {
        instrumentations.push(new option());
      } else if (option.instrumentationName) {
        instrumentations.push(option);
      }
    }
    return { instrumentations };
  }
  function enableInstrumentations(instrumentations, tracerProvider, meterProvider) {
    for (var i2 = 0, j2 = instrumentations.length; i2 < j2; i2++) {
      var instrumentation = instrumentations[i2];
      if (tracerProvider) {
        instrumentation.setTracerProvider(tracerProvider);
      }
      if (meterProvider) {
        instrumentation.setMeterProvider(meterProvider);
      }
      if (!instrumentation.getConfig().enabled) {
        instrumentation.enable();
      }
    }
  }
  function disableInstrumentations(instrumentations) {
    instrumentations.forEach(function(instrumentation) {
      return instrumentation.disable();
    });
  }

  // node_modules/@opentelemetry/instrumentation/build/esm/autoLoader.js
  function registerInstrumentations(options) {
    var instrumentations = parseInstrumentationOptions(options.instrumentations).instrumentations;
    var tracerProvider = options.tracerProvider || trace.getTracerProvider();
    var meterProvider = options.meterProvider || metrics.getMeterProvider();
    enableInstrumentations(instrumentations, tracerProvider, meterProvider);
    return function() {
      disableInstrumentations(instrumentations);
    };
  }

  // node_modules/@opentelemetry/instrumentation/build/esm/instrumentation.js
  var shimmer = __toESM(require_shimmer());
  var __assign2 = function() {
    __assign2 = Object.assign || function(t2) {
      for (var s2, i2 = 1, n2 = arguments.length; i2 < n2; i2++) {
        s2 = arguments[i2];
        for (var p2 in s2)
          if (Object.prototype.hasOwnProperty.call(s2, p2))
            t2[p2] = s2[p2];
      }
      return t2;
    };
    return __assign2.apply(this, arguments);
  };
  var InstrumentationAbstract = (
    /** @class */
    function() {
      function InstrumentationAbstract2(instrumentationName, instrumentationVersion, config) {
        if (config === void 0) {
          config = {};
        }
        this.instrumentationName = instrumentationName;
        this.instrumentationVersion = instrumentationVersion;
        this._wrap = shimmer.wrap;
        this._unwrap = shimmer.unwrap;
        this._massWrap = shimmer.massWrap;
        this._massUnwrap = shimmer.massUnwrap;
        this._config = __assign2({ enabled: true }, config);
        this._diag = diag2.createComponentLogger({
          namespace: instrumentationName
        });
        this._tracer = trace.getTracer(instrumentationName, instrumentationVersion);
        this._meter = metrics.getMeter(instrumentationName, instrumentationVersion);
        this._updateMetricInstruments();
      }
      Object.defineProperty(InstrumentationAbstract2.prototype, "meter", {
        /* Returns meter */
        get: function() {
          return this._meter;
        },
        enumerable: false,
        configurable: true
      });
      InstrumentationAbstract2.prototype.setMeterProvider = function(meterProvider) {
        this._meter = meterProvider.getMeter(this.instrumentationName, this.instrumentationVersion);
        this._updateMetricInstruments();
      };
      InstrumentationAbstract2.prototype._updateMetricInstruments = function() {
        return;
      };
      InstrumentationAbstract2.prototype.getConfig = function() {
        return this._config;
      };
      InstrumentationAbstract2.prototype.setConfig = function(config) {
        if (config === void 0) {
          config = {};
        }
        this._config = Object.assign({}, config);
      };
      InstrumentationAbstract2.prototype.setTracerProvider = function(tracerProvider) {
        this._tracer = tracerProvider.getTracer(this.instrumentationName, this.instrumentationVersion);
      };
      Object.defineProperty(InstrumentationAbstract2.prototype, "tracer", {
        /* Returns tracer */
        get: function() {
          return this._tracer;
        },
        enumerable: false,
        configurable: true
      });
      return InstrumentationAbstract2;
    }()
  );

  // node_modules/@opentelemetry/instrumentation/build/esm/platform/browser/instrumentation.js
  var __extends4 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var InstrumentationBase = (
    /** @class */
    function(_super) {
      __extends4(InstrumentationBase2, _super);
      function InstrumentationBase2(instrumentationName, instrumentationVersion, config) {
        if (config === void 0) {
          config = {};
        }
        var _this = _super.call(this, instrumentationName, instrumentationVersion, config) || this;
        if (_this._config.enabled) {
          _this.enable();
        }
        return _this;
      }
      return InstrumentationBase2;
    }(InstrumentationAbstract)
  );

  // node_modules/@opentelemetry/instrumentation/build/esm/utils.js
  function safeExecuteInTheMiddle(execute, onFinish, preventThrowingError) {
    var error;
    var result;
    try {
      result = execute();
    } catch (e2) {
      error = e2;
    } finally {
      onFinish(error, result);
      if (error && !preventThrowingError) {
        throw error;
      }
      return result;
    }
  }
  function isWrapped(func) {
    return typeof func === "function" && typeof func.__original === "function" && typeof func.__unwrap === "function" && func.__wrapped === true;
  }

  // node_modules/@opentelemetry/instrumentation-xml-http-request/build/esm/enums/EventNames.js
  var EventNames;
  (function(EventNames3) {
    EventNames3["METHOD_OPEN"] = "open";
    EventNames3["METHOD_SEND"] = "send";
    EventNames3["EVENT_ABORT"] = "abort";
    EventNames3["EVENT_ERROR"] = "error";
    EventNames3["EVENT_LOAD"] = "loaded";
    EventNames3["EVENT_TIMEOUT"] = "timeout";
  })(EventNames || (EventNames = {}));

  // node_modules/@opentelemetry/instrumentation-xml-http-request/build/esm/version.js
  var VERSION4 = "0.39.1";

  // node_modules/@opentelemetry/instrumentation-xml-http-request/build/esm/enums/AttributeNames.js
  var AttributeNames;
  (function(AttributeNames3) {
    AttributeNames3["HTTP_STATUS_TEXT"] = "http.status_text";
  })(AttributeNames || (AttributeNames = {}));

  // node_modules/@opentelemetry/instrumentation-xml-http-request/build/esm/xhr.js
  var __extends5 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var OBSERVER_WAIT_TIME_MS = 300;
  var XMLHttpRequestInstrumentation = (
    /** @class */
    function(_super) {
      __extends5(XMLHttpRequestInstrumentation2, _super);
      function XMLHttpRequestInstrumentation2(config) {
        var _this = _super.call(this, "@opentelemetry/instrumentation-xml-http-request", VERSION4, config) || this;
        _this.component = "xml-http-request";
        _this.version = VERSION4;
        _this.moduleName = _this.component;
        _this._tasksCount = 0;
        _this._xhrMem = /* @__PURE__ */ new WeakMap();
        _this._usedResources = /* @__PURE__ */ new WeakSet();
        return _this;
      }
      XMLHttpRequestInstrumentation2.prototype.init = function() {
      };
      XMLHttpRequestInstrumentation2.prototype._getConfig = function() {
        return this._config;
      };
      XMLHttpRequestInstrumentation2.prototype._addHeaders = function(xhr, spanUrl) {
        var url = parseUrl(spanUrl).href;
        if (!shouldPropagateTraceHeaders(url, this._getConfig().propagateTraceHeaderCorsUrls)) {
          var headers_1 = {};
          propagation.inject(context.active(), headers_1);
          if (Object.keys(headers_1).length > 0) {
            this._diag.debug("headers inject skipped due to CORS policy");
          }
          return;
        }
        var headers = {};
        propagation.inject(context.active(), headers);
        Object.keys(headers).forEach(function(key) {
          xhr.setRequestHeader(key, String(headers[key]));
        });
      };
      XMLHttpRequestInstrumentation2.prototype._addChildSpan = function(span, corsPreFlightRequest) {
        var _this = this;
        context.with(trace.setSpan(context.active(), span), function() {
          var childSpan = _this.tracer.startSpan("CORS Preflight", {
            startTime: corsPreFlightRequest[PerformanceTimingNames.FETCH_START]
          });
          addSpanNetworkEvents(childSpan, corsPreFlightRequest);
          childSpan.end(corsPreFlightRequest[PerformanceTimingNames.RESPONSE_END]);
        });
      };
      XMLHttpRequestInstrumentation2.prototype._addFinalSpanAttributes = function(span, xhrMem, spanUrl) {
        if (typeof spanUrl === "string") {
          var parsedUrl = parseUrl(spanUrl);
          if (xhrMem.status !== void 0) {
            span.setAttribute(SemanticAttributes.HTTP_STATUS_CODE, xhrMem.status);
          }
          if (xhrMem.statusText !== void 0) {
            span.setAttribute(AttributeNames.HTTP_STATUS_TEXT, xhrMem.statusText);
          }
          span.setAttribute(SemanticAttributes.HTTP_HOST, parsedUrl.host);
          span.setAttribute(SemanticAttributes.HTTP_SCHEME, parsedUrl.protocol.replace(":", ""));
          span.setAttribute(SemanticAttributes.HTTP_USER_AGENT, navigator.userAgent);
        }
      };
      XMLHttpRequestInstrumentation2.prototype._applyAttributesAfterXHR = function(span, xhr) {
        var _this = this;
        var applyCustomAttributesOnSpan = this._getConfig().applyCustomAttributesOnSpan;
        if (typeof applyCustomAttributesOnSpan === "function") {
          safeExecuteInTheMiddle(function() {
            return applyCustomAttributesOnSpan(span, xhr);
          }, function(error) {
            if (!error) {
              return;
            }
            _this._diag.error("applyCustomAttributesOnSpan", error);
          }, true);
        }
      };
      XMLHttpRequestInstrumentation2.prototype._addResourceObserver = function(xhr, spanUrl) {
        var xhrMem = this._xhrMem.get(xhr);
        if (!xhrMem || typeof PerformanceObserver !== "function" || typeof PerformanceResourceTiming !== "function") {
          return;
        }
        xhrMem.createdResources = {
          observer: new PerformanceObserver(function(list) {
            var entries = list.getEntries();
            var parsedUrl = parseUrl(spanUrl);
            entries.forEach(function(entry) {
              if (entry.initiatorType === "xmlhttprequest" && entry.name === parsedUrl.href) {
                if (xhrMem.createdResources) {
                  xhrMem.createdResources.entries.push(entry);
                }
              }
            });
          }),
          entries: []
        };
        xhrMem.createdResources.observer.observe({
          entryTypes: ["resource"]
        });
      };
      XMLHttpRequestInstrumentation2.prototype._clearResources = function() {
        if (this._tasksCount === 0 && this._getConfig().clearTimingResources) {
          otperformance.clearResourceTimings();
          this._xhrMem = /* @__PURE__ */ new WeakMap();
          this._usedResources = /* @__PURE__ */ new WeakSet();
        }
      };
      XMLHttpRequestInstrumentation2.prototype._findResourceAndAddNetworkEvents = function(xhrMem, span, spanUrl, startTime, endTime) {
        if (!spanUrl || !startTime || !endTime || !xhrMem.createdResources) {
          return;
        }
        var resources = xhrMem.createdResources.entries;
        if (!resources || !resources.length) {
          resources = otperformance.getEntriesByType("resource");
        }
        var resource = getResource(parseUrl(spanUrl).href, startTime, endTime, resources, this._usedResources);
        if (resource.mainRequest) {
          var mainRequest = resource.mainRequest;
          this._markResourceAsUsed(mainRequest);
          var corsPreFlightRequest = resource.corsPreFlightRequest;
          if (corsPreFlightRequest) {
            this._addChildSpan(span, corsPreFlightRequest);
            this._markResourceAsUsed(corsPreFlightRequest);
          }
          addSpanNetworkEvents(span, mainRequest);
        }
      };
      XMLHttpRequestInstrumentation2.prototype._cleanPreviousSpanInformation = function(xhr) {
        var xhrMem = this._xhrMem.get(xhr);
        if (xhrMem) {
          var callbackToRemoveEvents = xhrMem.callbackToRemoveEvents;
          if (callbackToRemoveEvents) {
            callbackToRemoveEvents();
          }
          this._xhrMem.delete(xhr);
        }
      };
      XMLHttpRequestInstrumentation2.prototype._createSpan = function(xhr, url, method) {
        var _a2;
        if (isUrlIgnored(url, this._getConfig().ignoreUrls)) {
          this._diag.debug("ignoring span as url matches ignored url");
          return;
        }
        var spanName = method.toUpperCase();
        var currentSpan = this.tracer.startSpan(spanName, {
          kind: SpanKind.CLIENT,
          attributes: (_a2 = {}, _a2[SemanticAttributes.HTTP_METHOD] = method, _a2[SemanticAttributes.HTTP_URL] = parseUrl(url).toString(), _a2)
        });
        currentSpan.addEvent(EventNames.METHOD_OPEN);
        this._cleanPreviousSpanInformation(xhr);
        this._xhrMem.set(xhr, {
          span: currentSpan,
          spanUrl: url
        });
        return currentSpan;
      };
      XMLHttpRequestInstrumentation2.prototype._markResourceAsUsed = function(resource) {
        this._usedResources.add(resource);
      };
      XMLHttpRequestInstrumentation2.prototype._patchOpen = function() {
        var _this = this;
        return function(original) {
          var plugin = _this;
          return function patchOpen() {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
              args[_i] = arguments[_i];
            }
            var method = args[0];
            var url = args[1];
            plugin._createSpan(this, url, method);
            return original.apply(this, args);
          };
        };
      };
      XMLHttpRequestInstrumentation2.prototype._patchSend = function() {
        var plugin = this;
        function endSpanTimeout(eventName, xhrMem, performanceEndTime, endTime) {
          var callbackToRemoveEvents = xhrMem.callbackToRemoveEvents;
          if (typeof callbackToRemoveEvents === "function") {
            callbackToRemoveEvents();
          }
          var span = xhrMem.span, spanUrl = xhrMem.spanUrl, sendStartTime = xhrMem.sendStartTime;
          if (span) {
            plugin._findResourceAndAddNetworkEvents(xhrMem, span, spanUrl, sendStartTime, performanceEndTime);
            span.addEvent(eventName, endTime);
            plugin._addFinalSpanAttributes(span, xhrMem, spanUrl);
            span.end(endTime);
            plugin._tasksCount--;
          }
          plugin._clearResources();
        }
        function endSpan(eventName, xhr) {
          var xhrMem = plugin._xhrMem.get(xhr);
          if (!xhrMem) {
            return;
          }
          xhrMem.status = xhr.status;
          xhrMem.statusText = xhr.statusText;
          plugin._xhrMem.delete(xhr);
          if (xhrMem.span) {
            plugin._applyAttributesAfterXHR(xhrMem.span, xhr);
          }
          var performanceEndTime = hrTime();
          var endTime = Date.now();
          setTimeout(function() {
            endSpanTimeout(eventName, xhrMem, performanceEndTime, endTime);
          }, OBSERVER_WAIT_TIME_MS);
        }
        function onError() {
          endSpan(EventNames.EVENT_ERROR, this);
        }
        function onAbort() {
          endSpan(EventNames.EVENT_ABORT, this);
        }
        function onTimeout() {
          endSpan(EventNames.EVENT_TIMEOUT, this);
        }
        function onLoad() {
          if (this.status < 299) {
            endSpan(EventNames.EVENT_LOAD, this);
          } else {
            endSpan(EventNames.EVENT_ERROR, this);
          }
        }
        function unregister(xhr) {
          xhr.removeEventListener("abort", onAbort);
          xhr.removeEventListener("error", onError);
          xhr.removeEventListener("load", onLoad);
          xhr.removeEventListener("timeout", onTimeout);
          var xhrMem = plugin._xhrMem.get(xhr);
          if (xhrMem) {
            xhrMem.callbackToRemoveEvents = void 0;
          }
        }
        return function(original) {
          return function patchSend() {
            var _this = this;
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
              args[_i] = arguments[_i];
            }
            var xhrMem = plugin._xhrMem.get(this);
            if (!xhrMem) {
              return original.apply(this, args);
            }
            var currentSpan = xhrMem.span;
            var spanUrl = xhrMem.spanUrl;
            if (currentSpan && spanUrl) {
              context.with(trace.setSpan(context.active(), currentSpan), function() {
                plugin._tasksCount++;
                xhrMem.sendStartTime = hrTime();
                currentSpan.addEvent(EventNames.METHOD_SEND);
                _this.addEventListener("abort", onAbort);
                _this.addEventListener("error", onError);
                _this.addEventListener("load", onLoad);
                _this.addEventListener("timeout", onTimeout);
                xhrMem.callbackToRemoveEvents = function() {
                  unregister(_this);
                  if (xhrMem.createdResources) {
                    xhrMem.createdResources.observer.disconnect();
                  }
                };
                plugin._addHeaders(_this, spanUrl);
                plugin._addResourceObserver(_this, spanUrl);
              });
            }
            return original.apply(this, args);
          };
        };
      };
      XMLHttpRequestInstrumentation2.prototype.enable = function() {
        this._diag.debug("applying patch to", this.moduleName, this.version);
        if (isWrapped(XMLHttpRequest.prototype.open)) {
          this._unwrap(XMLHttpRequest.prototype, "open");
          this._diag.debug("removing previous patch from method open");
        }
        if (isWrapped(XMLHttpRequest.prototype.send)) {
          this._unwrap(XMLHttpRequest.prototype, "send");
          this._diag.debug("removing previous patch from method send");
        }
        this._wrap(XMLHttpRequest.prototype, "open", this._patchOpen());
        this._wrap(XMLHttpRequest.prototype, "send", this._patchSend());
      };
      XMLHttpRequestInstrumentation2.prototype.disable = function() {
        this._diag.debug("removing patch from", this.moduleName, this.version);
        this._unwrap(XMLHttpRequest.prototype, "open");
        this._unwrap(XMLHttpRequest.prototype, "send");
        this._tasksCount = 0;
        this._xhrMem = /* @__PURE__ */ new WeakMap();
        this._usedResources = /* @__PURE__ */ new WeakSet();
      };
      return XMLHttpRequestInstrumentation2;
    }(InstrumentationBase)
  );

  // node_modules/@opentelemetry/instrumentation-document-load/build/esm/enums/AttributeNames.js
  var AttributeNames2;
  (function(AttributeNames3) {
    AttributeNames3["DOCUMENT_LOAD"] = "documentLoad";
    AttributeNames3["DOCUMENT_FETCH"] = "documentFetch";
    AttributeNames3["RESOURCE_FETCH"] = "resourceFetch";
  })(AttributeNames2 || (AttributeNames2 = {}));

  // node_modules/@opentelemetry/instrumentation-document-load/build/esm/version.js
  var VERSION5 = "0.32.2";

  // node_modules/@opentelemetry/instrumentation-document-load/build/esm/enums/EventNames.js
  var EventNames2;
  (function(EventNames3) {
    EventNames3["FIRST_PAINT"] = "firstPaint";
    EventNames3["FIRST_CONTENTFUL_PAINT"] = "firstContentfulPaint";
  })(EventNames2 || (EventNames2 = {}));

  // node_modules/@opentelemetry/instrumentation-document-load/build/esm/utils.js
  var getPerformanceNavigationEntries = function() {
    var _a2, _b;
    var entries = {};
    var performanceNavigationTiming = (_b = (_a2 = otperformance).getEntriesByType) === null || _b === void 0 ? void 0 : _b.call(_a2, "navigation")[0];
    if (performanceNavigationTiming) {
      var keys = Object.values(PerformanceTimingNames);
      keys.forEach(function(key) {
        if (hasKey(performanceNavigationTiming, key)) {
          var value = performanceNavigationTiming[key];
          if (typeof value === "number") {
            entries[key] = value;
          }
        }
      });
    } else {
      var perf = otperformance;
      var performanceTiming_1 = perf.timing;
      if (performanceTiming_1) {
        var keys = Object.values(PerformanceTimingNames);
        keys.forEach(function(key) {
          if (hasKey(performanceTiming_1, key)) {
            var value = performanceTiming_1[key];
            if (typeof value === "number") {
              entries[key] = value;
            }
          }
        });
      }
    }
    return entries;
  };
  var performancePaintNames = {
    "first-paint": EventNames2.FIRST_PAINT,
    "first-contentful-paint": EventNames2.FIRST_CONTENTFUL_PAINT
  };
  var addSpanPerformancePaintEvents = function(span) {
    var _a2, _b;
    var performancePaintTiming = (_b = (_a2 = otperformance).getEntriesByType) === null || _b === void 0 ? void 0 : _b.call(_a2, "paint");
    if (performancePaintTiming) {
      performancePaintTiming.forEach(function(_a3) {
        var name = _a3.name, startTime = _a3.startTime;
        if (hasKey(performancePaintNames, name)) {
          span.addEvent(performancePaintNames[name], startTime);
        }
      });
    }
  };

  // node_modules/@opentelemetry/instrumentation-document-load/build/esm/instrumentation.js
  var __extends6 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var DocumentLoadInstrumentation = (
    /** @class */
    function(_super) {
      __extends6(DocumentLoadInstrumentation2, _super);
      function DocumentLoadInstrumentation2(config) {
        if (config === void 0) {
          config = {};
        }
        var _this = _super.call(this, "@opentelemetry/instrumentation-document-load", VERSION5, config) || this;
        _this.component = "document-load";
        _this.version = "1";
        _this.moduleName = _this.component;
        return _this;
      }
      DocumentLoadInstrumentation2.prototype.init = function() {
      };
      DocumentLoadInstrumentation2.prototype._onDocumentLoaded = function() {
        var _this = this;
        window.setTimeout(function() {
          _this._collectPerformance();
        });
      };
      DocumentLoadInstrumentation2.prototype._addResourcesSpans = function(rootSpan) {
        var _this = this;
        var _a2, _b;
        var resources = (_b = (_a2 = otperformance).getEntriesByType) === null || _b === void 0 ? void 0 : _b.call(_a2, "resource");
        if (resources) {
          resources.forEach(function(resource) {
            _this._initResourceSpan(resource, rootSpan);
          });
        }
      };
      DocumentLoadInstrumentation2.prototype._collectPerformance = function() {
        var _this = this;
        var metaElement = Array.from(document.getElementsByTagName("meta")).find(function(e2) {
          return e2.getAttribute("name") === TRACE_PARENT_HEADER;
        });
        var entries = getPerformanceNavigationEntries();
        var traceparent = metaElement && metaElement.content || "";
        context.with(propagation.extract(ROOT_CONTEXT, { traceparent }), function() {
          var _a2;
          var rootSpan = _this._startSpan(AttributeNames2.DOCUMENT_LOAD, PerformanceTimingNames.FETCH_START, entries);
          if (!rootSpan) {
            return;
          }
          context.with(trace.setSpan(context.active(), rootSpan), function() {
            var fetchSpan = _this._startSpan(AttributeNames2.DOCUMENT_FETCH, PerformanceTimingNames.FETCH_START, entries);
            if (fetchSpan) {
              fetchSpan.setAttribute(SemanticAttributes.HTTP_URL, location.href);
              context.with(trace.setSpan(context.active(), fetchSpan), function() {
                var _a3;
                addSpanNetworkEvents(fetchSpan, entries);
                _this._addCustomAttributesOnSpan(fetchSpan, (_a3 = _this._getConfig().applyCustomAttributesOnSpan) === null || _a3 === void 0 ? void 0 : _a3.documentFetch);
                _this._endSpan(fetchSpan, PerformanceTimingNames.RESPONSE_END, entries);
              });
            }
          });
          rootSpan.setAttribute(SemanticAttributes.HTTP_URL, location.href);
          rootSpan.setAttribute(SemanticAttributes.HTTP_USER_AGENT, navigator.userAgent);
          _this._addResourcesSpans(rootSpan);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.FETCH_START, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.UNLOAD_EVENT_START, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.UNLOAD_EVENT_END, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.DOM_INTERACTIVE, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.DOM_CONTENT_LOADED_EVENT_START, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.DOM_CONTENT_LOADED_EVENT_END, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.DOM_COMPLETE, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.LOAD_EVENT_START, entries);
          addSpanNetworkEvent(rootSpan, PerformanceTimingNames.LOAD_EVENT_END, entries);
          addSpanPerformancePaintEvents(rootSpan);
          _this._addCustomAttributesOnSpan(rootSpan, (_a2 = _this._getConfig().applyCustomAttributesOnSpan) === null || _a2 === void 0 ? void 0 : _a2.documentLoad);
          _this._endSpan(rootSpan, PerformanceTimingNames.LOAD_EVENT_END, entries);
        });
      };
      DocumentLoadInstrumentation2.prototype._endSpan = function(span, performanceName, entries) {
        if (span) {
          if (hasKey(entries, performanceName)) {
            span.end(entries[performanceName]);
          } else {
            span.end();
          }
        }
      };
      DocumentLoadInstrumentation2.prototype._initResourceSpan = function(resource, parentSpan) {
        var _a2;
        var span = this._startSpan(AttributeNames2.RESOURCE_FETCH, PerformanceTimingNames.FETCH_START, resource, parentSpan);
        if (span) {
          span.setAttribute(SemanticAttributes.HTTP_URL, resource.name);
          addSpanNetworkEvents(span, resource);
          this._addCustomAttributesOnSpan(span, (_a2 = this._getConfig().applyCustomAttributesOnSpan) === null || _a2 === void 0 ? void 0 : _a2.resourceFetch);
          this._endSpan(span, PerformanceTimingNames.RESPONSE_END, resource);
        }
      };
      DocumentLoadInstrumentation2.prototype._startSpan = function(spanName, performanceName, entries, parentSpan) {
        if (hasKey(entries, performanceName) && typeof entries[performanceName] === "number") {
          var span = this.tracer.startSpan(spanName, {
            startTime: entries[performanceName]
          }, parentSpan ? trace.setSpan(context.active(), parentSpan) : void 0);
          return span;
        }
        return void 0;
      };
      DocumentLoadInstrumentation2.prototype._waitForPageLoad = function() {
        if (window.document.readyState === "complete") {
          this._onDocumentLoaded();
        } else {
          this._onDocumentLoaded = this._onDocumentLoaded.bind(this);
          window.addEventListener("load", this._onDocumentLoaded);
        }
      };
      DocumentLoadInstrumentation2.prototype._getConfig = function() {
        return this._config;
      };
      DocumentLoadInstrumentation2.prototype._addCustomAttributesOnSpan = function(span, applyCustomAttributesOnSpan) {
        var _this = this;
        if (applyCustomAttributesOnSpan) {
          safeExecuteInTheMiddle(function() {
            return applyCustomAttributesOnSpan(span);
          }, function(error) {
            if (!error) {
              return;
            }
            _this._diag.error("addCustomAttributesOnSpan", error);
          }, true);
        }
      };
      DocumentLoadInstrumentation2.prototype.enable = function() {
        window.removeEventListener("load", this._onDocumentLoaded);
        this._waitForPageLoad();
      };
      DocumentLoadInstrumentation2.prototype.disable = function() {
        window.removeEventListener("load", this._onDocumentLoaded);
      };
      return DocumentLoadInstrumentation2;
    }(InstrumentationBase)
  );

  // node_modules/@opentelemetry/otlp-exporter-base/build/esm/util.js
  var __read12 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  var DEFAULT_TRACE_TIMEOUT = 1e4;
  var DEFAULT_EXPORT_MAX_ATTEMPTS = 5;
  var DEFAULT_EXPORT_INITIAL_BACKOFF = 1e3;
  var DEFAULT_EXPORT_MAX_BACKOFF = 5e3;
  var DEFAULT_EXPORT_BACKOFF_MULTIPLIER = 1.5;
  function parseHeaders(partialHeaders) {
    if (partialHeaders === void 0) {
      partialHeaders = {};
    }
    var headers = {};
    Object.entries(partialHeaders).forEach(function(_a2) {
      var _b = __read12(_a2, 2), key = _b[0], value = _b[1];
      if (typeof value !== "undefined") {
        headers[key] = String(value);
      } else {
        diag2.warn('Header "' + key + '" has wrong value and will be ignored');
      }
    });
    return headers;
  }
  function appendResourcePathToUrl(url, path) {
    if (!url.endsWith("/")) {
      url = url + "/";
    }
    return url + path;
  }
  function appendRootPathToUrlIfNeeded(url) {
    try {
      var parsedUrl = new URL(url);
      if (parsedUrl.pathname === "") {
        parsedUrl.pathname = parsedUrl.pathname + "/";
      }
      return parsedUrl.toString();
    } catch (_a2) {
      diag2.warn("Could not parse export URL: '" + url + "'");
      return url;
    }
  }
  function configureExporterTimeout(timeoutMillis) {
    if (typeof timeoutMillis === "number") {
      if (timeoutMillis <= 0) {
        return invalidTimeout(timeoutMillis, DEFAULT_TRACE_TIMEOUT);
      }
      return timeoutMillis;
    } else {
      return getExporterTimeoutFromEnv();
    }
  }
  function getExporterTimeoutFromEnv() {
    var _a2;
    var definedTimeout = Number((_a2 = getEnv().OTEL_EXPORTER_OTLP_TRACES_TIMEOUT) !== null && _a2 !== void 0 ? _a2 : getEnv().OTEL_EXPORTER_OTLP_TIMEOUT);
    if (definedTimeout <= 0) {
      return invalidTimeout(definedTimeout, DEFAULT_TRACE_TIMEOUT);
    } else {
      return definedTimeout;
    }
  }
  function invalidTimeout(timeout, defaultTimeout) {
    diag2.warn("Timeout must be greater than 0", timeout);
    return defaultTimeout;
  }
  function isExportRetryable(statusCode) {
    var retryCodes = [429, 502, 503, 504];
    return retryCodes.includes(statusCode);
  }
  function parseRetryAfterToMills(retryAfter) {
    if (retryAfter == null) {
      return -1;
    }
    var seconds = Number.parseInt(retryAfter, 10);
    if (Number.isInteger(seconds)) {
      return seconds > 0 ? seconds * 1e3 : -1;
    }
    var delay = new Date(retryAfter).getTime() - Date.now();
    if (delay >= 0) {
      return delay;
    }
    return 0;
  }

  // node_modules/@opentelemetry/otlp-exporter-base/build/esm/OTLPExporterBase.js
  var OTLPExporterBase = (
    /** @class */
    function() {
      function OTLPExporterBase2(config) {
        if (config === void 0) {
          config = {};
        }
        this._sendingPromises = [];
        this.url = this.getDefaultUrl(config);
        if (typeof config.hostname === "string") {
          this.hostname = config.hostname;
        }
        this.shutdown = this.shutdown.bind(this);
        this._shutdownOnce = new BindOnceFuture(this._shutdown, this);
        this._concurrencyLimit = typeof config.concurrencyLimit === "number" ? config.concurrencyLimit : Infinity;
        this.timeoutMillis = configureExporterTimeout(config.timeoutMillis);
        this.onInit(config);
      }
      OTLPExporterBase2.prototype.export = function(items, resultCallback) {
        if (this._shutdownOnce.isCalled) {
          resultCallback({
            code: ExportResultCode.FAILED,
            error: new Error("Exporter has been shutdown")
          });
          return;
        }
        if (this._sendingPromises.length >= this._concurrencyLimit) {
          resultCallback({
            code: ExportResultCode.FAILED,
            error: new Error("Concurrent export limit reached")
          });
          return;
        }
        this._export(items).then(function() {
          resultCallback({ code: ExportResultCode.SUCCESS });
        }).catch(function(error) {
          resultCallback({ code: ExportResultCode.FAILED, error });
        });
      };
      OTLPExporterBase2.prototype._export = function(items) {
        var _this = this;
        return new Promise(function(resolve, reject) {
          try {
            diag2.debug("items to be sent", items);
            _this.send(items, resolve, reject);
          } catch (e2) {
            reject(e2);
          }
        });
      };
      OTLPExporterBase2.prototype.shutdown = function() {
        return this._shutdownOnce.call();
      };
      OTLPExporterBase2.prototype._shutdown = function() {
        diag2.debug("shutdown started");
        this.onShutdown();
        return Promise.all(this._sendingPromises).then(function() {
        });
      };
      return OTLPExporterBase2;
    }()
  );

  // node_modules/@opentelemetry/otlp-exporter-base/build/esm/types.js
  var __extends7 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var OTLPExporterError = (
    /** @class */
    function(_super) {
      __extends7(OTLPExporterError2, _super);
      function OTLPExporterError2(message, code, data) {
        var _this = _super.call(this, message) || this;
        _this.name = "OTLPExporterError";
        _this.data = data;
        _this.code = code;
        return _this;
      }
      return OTLPExporterError2;
    }(Error)
  );

  // node_modules/@opentelemetry/otlp-exporter-base/build/esm/platform/browser/util.js
  var __assign3 = function() {
    __assign3 = Object.assign || function(t2) {
      for (var s2, i2 = 1, n2 = arguments.length; i2 < n2; i2++) {
        s2 = arguments[i2];
        for (var p2 in s2)
          if (Object.prototype.hasOwnProperty.call(s2, p2))
            t2[p2] = s2[p2];
      }
      return t2;
    };
    return __assign3.apply(this, arguments);
  };
  var __read13 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  function sendWithBeacon(body, url, blobPropertyBag, onSuccess, onError) {
    if (navigator.sendBeacon(url, new Blob([body], blobPropertyBag))) {
      diag2.debug("sendBeacon - can send", body);
      onSuccess();
    } else {
      var error = new OTLPExporterError("sendBeacon - cannot send " + body);
      onError(error);
    }
  }
  function sendWithXhr(body, url, headers, exporterTimeout, onSuccess, onError) {
    var retryTimer;
    var xhr;
    var reqIsDestroyed = false;
    var exporterTimer = setTimeout(function() {
      clearTimeout(retryTimer);
      reqIsDestroyed = true;
      if (xhr.readyState === XMLHttpRequest.DONE) {
        var err = new OTLPExporterError("Request Timeout");
        onError(err);
      } else {
        xhr.abort();
      }
    }, exporterTimeout);
    var sendWithRetry = function(retries, minDelay) {
      if (retries === void 0) {
        retries = DEFAULT_EXPORT_MAX_ATTEMPTS;
      }
      if (minDelay === void 0) {
        minDelay = DEFAULT_EXPORT_INITIAL_BACKOFF;
      }
      xhr = new XMLHttpRequest();
      xhr.open("POST", url);
      var defaultHeaders = {
        Accept: "application/json",
        "Content-Type": "application/json"
      };
      Object.entries(__assign3(__assign3({}, defaultHeaders), headers)).forEach(function(_a2) {
        var _b = __read13(_a2, 2), k2 = _b[0], v2 = _b[1];
        xhr.setRequestHeader(k2, v2);
      });
      xhr.send(body);
      xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE && reqIsDestroyed === false) {
          if (xhr.status >= 200 && xhr.status <= 299) {
            diag2.debug("xhr success", body);
            onSuccess();
            clearTimeout(exporterTimer);
            clearTimeout(retryTimer);
          } else if (xhr.status && isExportRetryable(xhr.status) && retries > 0) {
            var retryTime = void 0;
            minDelay = DEFAULT_EXPORT_BACKOFF_MULTIPLIER * minDelay;
            if (xhr.getResponseHeader("Retry-After")) {
              retryTime = parseRetryAfterToMills(xhr.getResponseHeader("Retry-After"));
            } else {
              retryTime = Math.round(Math.random() * (DEFAULT_EXPORT_MAX_BACKOFF - minDelay) + minDelay);
            }
            retryTimer = setTimeout(function() {
              sendWithRetry(retries - 1, minDelay);
            }, retryTime);
          } else {
            var error = new OTLPExporterError("Failed to export with XHR (status: " + xhr.status + ")", xhr.status);
            onError(error);
            clearTimeout(exporterTimer);
            clearTimeout(retryTimer);
          }
        }
      };
      xhr.onabort = function() {
        if (reqIsDestroyed) {
          var err = new OTLPExporterError("Request Timeout");
          onError(err);
        }
        clearTimeout(exporterTimer);
        clearTimeout(retryTimer);
      };
      xhr.onerror = function() {
        if (reqIsDestroyed) {
          var err = new OTLPExporterError("Request Timeout");
          onError(err);
        }
        clearTimeout(exporterTimer);
        clearTimeout(retryTimer);
      };
    };
    sendWithRetry();
  }

  // node_modules/@opentelemetry/otlp-exporter-base/build/esm/platform/browser/OTLPExporterBrowserBase.js
  var __extends8 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var OTLPExporterBrowserBase = (
    /** @class */
    function(_super) {
      __extends8(OTLPExporterBrowserBase2, _super);
      function OTLPExporterBrowserBase2(config) {
        if (config === void 0) {
          config = {};
        }
        var _this = _super.call(this, config) || this;
        _this._useXHR = false;
        _this._useXHR = !!config.headers || typeof navigator.sendBeacon !== "function";
        if (_this._useXHR) {
          _this._headers = Object.assign({}, parseHeaders(config.headers), utils_exports.parseKeyPairsIntoRecord(getEnv().OTEL_EXPORTER_OTLP_HEADERS));
        } else {
          _this._headers = {};
        }
        return _this;
      }
      OTLPExporterBrowserBase2.prototype.onInit = function() {
        window.addEventListener("unload", this.shutdown);
      };
      OTLPExporterBrowserBase2.prototype.onShutdown = function() {
        window.removeEventListener("unload", this.shutdown);
      };
      OTLPExporterBrowserBase2.prototype.send = function(items, onSuccess, onError) {
        var _this = this;
        if (this._shutdownOnce.isCalled) {
          diag2.debug("Shutdown already started. Cannot send objects");
          return;
        }
        var serviceRequest = this.convert(items);
        var body = JSON.stringify(serviceRequest);
        var promise = new Promise(function(resolve, reject) {
          if (_this._useXHR) {
            sendWithXhr(body, _this.url, _this._headers, _this.timeoutMillis, resolve, reject);
          } else {
            sendWithBeacon(body, _this.url, { type: "application/json" }, resolve, reject);
          }
        }).then(onSuccess, onError);
        this._sendingPromises.push(promise);
        var popPromise = function() {
          var index = _this._sendingPromises.indexOf(promise);
          _this._sendingPromises.splice(index, 1);
        };
        promise.then(popPromise, popPromise);
      };
      return OTLPExporterBrowserBase2;
    }(OTLPExporterBase)
  );

  // node_modules/@opentelemetry/otlp-transformer/build/esm/common/internal.js
  var __read14 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  function toAttributes(attributes) {
    return Object.keys(attributes).map(function(key) {
      return toKeyValue(key, attributes[key]);
    });
  }
  function toKeyValue(key, value) {
    return {
      key,
      value: toAnyValue(value)
    };
  }
  function toAnyValue(value) {
    var t2 = typeof value;
    if (t2 === "string")
      return { stringValue: value };
    if (t2 === "number") {
      if (!Number.isInteger(value))
        return { doubleValue: value };
      return { intValue: value };
    }
    if (t2 === "boolean")
      return { boolValue: value };
    if (value instanceof Uint8Array)
      return { bytesValue: value };
    if (Array.isArray(value))
      return { arrayValue: { values: value.map(toAnyValue) } };
    if (t2 === "object" && value != null)
      return {
        kvlistValue: {
          values: Object.entries(value).map(function(_a2) {
            var _b = __read14(_a2, 2), k2 = _b[0], v2 = _b[1];
            return toKeyValue(k2, v2);
          })
        }
      };
    return {};
  }

  // node_modules/@opentelemetry/otlp-transformer/build/esm/trace/internal.js
  function sdkSpanToOtlpSpan(span, useHex) {
    var _a2;
    var ctx = span.spanContext();
    var status = span.status;
    var parentSpanId = useHex ? span.parentSpanId : span.parentSpanId != null ? hexToBase64(span.parentSpanId) : void 0;
    return {
      traceId: useHex ? ctx.traceId : hexToBase64(ctx.traceId),
      spanId: useHex ? ctx.spanId : hexToBase64(ctx.spanId),
      parentSpanId,
      traceState: (_a2 = ctx.traceState) === null || _a2 === void 0 ? void 0 : _a2.serialize(),
      name: span.name,
      // Span kind is offset by 1 because the API does not define a value for unset
      kind: span.kind == null ? 0 : span.kind + 1,
      startTimeUnixNano: hrTimeToNanoseconds(span.startTime),
      endTimeUnixNano: hrTimeToNanoseconds(span.endTime),
      attributes: toAttributes(span.attributes),
      droppedAttributesCount: span.droppedAttributesCount,
      events: span.events.map(toOtlpSpanEvent),
      droppedEventsCount: span.droppedEventsCount,
      status: {
        // API and proto enums share the same values
        code: status.code,
        message: status.message
      },
      links: span.links.map(function(link) {
        return toOtlpLink(link, useHex);
      }),
      droppedLinksCount: span.droppedLinksCount
    };
  }
  function toOtlpLink(link, useHex) {
    var _a2;
    return {
      attributes: link.attributes ? toAttributes(link.attributes) : [],
      spanId: useHex ? link.context.spanId : hexToBase64(link.context.spanId),
      traceId: useHex ? link.context.traceId : hexToBase64(link.context.traceId),
      traceState: (_a2 = link.context.traceState) === null || _a2 === void 0 ? void 0 : _a2.serialize(),
      droppedAttributesCount: link.droppedAttributesCount || 0
    };
  }
  function toOtlpSpanEvent(timedEvent) {
    return {
      attributes: timedEvent.attributes ? toAttributes(timedEvent.attributes) : [],
      name: timedEvent.name,
      timeUnixNano: hrTimeToNanoseconds(timedEvent.time),
      droppedAttributesCount: timedEvent.droppedAttributesCount || 0
    };
  }

  // node_modules/@opentelemetry/otlp-transformer/build/esm/trace/index.js
  var __values8 = function(o2) {
    var s2 = typeof Symbol === "function" && Symbol.iterator, m2 = s2 && o2[s2], i2 = 0;
    if (m2)
      return m2.call(o2);
    if (o2 && typeof o2.length === "number")
      return {
        next: function() {
          if (o2 && i2 >= o2.length)
            o2 = void 0;
          return { value: o2 && o2[i2++], done: !o2 };
        }
      };
    throw new TypeError(s2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
  };
  var __read15 = function(o2, n2) {
    var m2 = typeof Symbol === "function" && o2[Symbol.iterator];
    if (!m2)
      return o2;
    var i2 = m2.call(o2), r2, ar2 = [], e2;
    try {
      while ((n2 === void 0 || n2-- > 0) && !(r2 = i2.next()).done)
        ar2.push(r2.value);
    } catch (error) {
      e2 = { error };
    } finally {
      try {
        if (r2 && !r2.done && (m2 = i2["return"]))
          m2.call(i2);
      } finally {
        if (e2)
          throw e2.error;
      }
    }
    return ar2;
  };
  function createExportTraceServiceRequest(spans, useHex) {
    return {
      resourceSpans: spanRecordsToResourceSpans(spans, useHex)
    };
  }
  function createResourceMap(readableSpans) {
    var e_1, _a2;
    var resourceMap = /* @__PURE__ */ new Map();
    try {
      for (var readableSpans_1 = __values8(readableSpans), readableSpans_1_1 = readableSpans_1.next(); !readableSpans_1_1.done; readableSpans_1_1 = readableSpans_1.next()) {
        var record = readableSpans_1_1.value;
        var ilmMap = resourceMap.get(record.resource);
        if (!ilmMap) {
          ilmMap = /* @__PURE__ */ new Map();
          resourceMap.set(record.resource, ilmMap);
        }
        var instrumentationLibraryKey = record.instrumentationLibrary.name + "@" + (record.instrumentationLibrary.version || "") + ":" + (record.instrumentationLibrary.schemaUrl || "");
        var records = ilmMap.get(instrumentationLibraryKey);
        if (!records) {
          records = [];
          ilmMap.set(instrumentationLibraryKey, records);
        }
        records.push(record);
      }
    } catch (e_1_1) {
      e_1 = { error: e_1_1 };
    } finally {
      try {
        if (readableSpans_1_1 && !readableSpans_1_1.done && (_a2 = readableSpans_1.return))
          _a2.call(readableSpans_1);
      } finally {
        if (e_1)
          throw e_1.error;
      }
    }
    return resourceMap;
  }
  function spanRecordsToResourceSpans(readableSpans, useHex) {
    var resourceMap = createResourceMap(readableSpans);
    var out = [];
    var entryIterator = resourceMap.entries();
    var entry = entryIterator.next();
    while (!entry.done) {
      var _a2 = __read15(entry.value, 2), resource = _a2[0], ilmMap = _a2[1];
      var scopeResourceSpans = [];
      var ilmIterator = ilmMap.values();
      var ilmEntry = ilmIterator.next();
      while (!ilmEntry.done) {
        var scopeSpans = ilmEntry.value;
        if (scopeSpans.length > 0) {
          var _b = scopeSpans[0].instrumentationLibrary, name_1 = _b.name, version = _b.version, schemaUrl = _b.schemaUrl;
          var spans = scopeSpans.map(function(readableSpan) {
            return sdkSpanToOtlpSpan(readableSpan, useHex);
          });
          scopeResourceSpans.push({
            scope: { name: name_1, version },
            spans,
            schemaUrl
          });
        }
        ilmEntry = ilmIterator.next();
      }
      var transformedSpans = {
        resource: {
          attributes: toAttributes(resource.attributes),
          droppedAttributesCount: 0
        },
        scopeSpans: scopeResourceSpans,
        schemaUrl: void 0
      };
      out.push(transformedSpans);
      entry = entryIterator.next();
    }
    return out;
  }

  // node_modules/@opentelemetry/exporter-trace-otlp-http/build/esm/platform/browser/OTLPTraceExporter.js
  var __extends9 = function() {
    var extendStatics = function(d2, b2) {
      extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d3, b3) {
        d3.__proto__ = b3;
      } || function(d3, b3) {
        for (var p2 in b3)
          if (Object.prototype.hasOwnProperty.call(b3, p2))
            d3[p2] = b3[p2];
      };
      return extendStatics(d2, b2);
    };
    return function(d2, b2) {
      if (typeof b2 !== "function" && b2 !== null)
        throw new TypeError("Class extends value " + String(b2) + " is not a constructor or null");
      extendStatics(d2, b2);
      function __() {
        this.constructor = d2;
      }
      d2.prototype = b2 === null ? Object.create(b2) : (__.prototype = b2.prototype, new __());
    };
  }();
  var DEFAULT_COLLECTOR_RESOURCE_PATH = "v1/traces";
  var DEFAULT_COLLECTOR_URL = "http://localhost:4318/" + DEFAULT_COLLECTOR_RESOURCE_PATH;
  var OTLPTraceExporter = (
    /** @class */
    function(_super) {
      __extends9(OTLPTraceExporter2, _super);
      function OTLPTraceExporter2(config) {
        if (config === void 0) {
          config = {};
        }
        var _this = _super.call(this, config) || this;
        _this._headers = Object.assign(_this._headers, utils_exports.parseKeyPairsIntoRecord(getEnv().OTEL_EXPORTER_OTLP_TRACES_HEADERS));
        return _this;
      }
      OTLPTraceExporter2.prototype.convert = function(spans) {
        return createExportTraceServiceRequest(spans, true);
      };
      OTLPTraceExporter2.prototype.getDefaultUrl = function(config) {
        return typeof config.url === "string" ? config.url : getEnv().OTEL_EXPORTER_OTLP_TRACES_ENDPOINT.length > 0 ? appendRootPathToUrlIfNeeded(getEnv().OTEL_EXPORTER_OTLP_TRACES_ENDPOINT) : getEnv().OTEL_EXPORTER_OTLP_ENDPOINT.length > 0 ? appendResourcePathToUrl(getEnv().OTEL_EXPORTER_OTLP_ENDPOINT, DEFAULT_COLLECTOR_RESOURCE_PATH) : DEFAULT_COLLECTOR_URL;
      };
      return OTLPTraceExporter2;
    }(OTLPExporterBrowserBase)
  );

  // node_modules/@opentelemetry/context-zone-peer-dep/build/esm/util.js
  function isListenerObject(obj) {
    if (obj === void 0) {
      obj = {};
    }
    return typeof obj.addEventListener === "function" && typeof obj.removeEventListener === "function";
  }

  // node_modules/@opentelemetry/context-zone-peer-dep/build/esm/ZoneContextManager.js
  var ZONE_CONTEXT_KEY = "OT_ZONE_CONTEXT";
  var ZoneContextManager = (
    /** @class */
    function() {
      function ZoneContextManager2() {
        this._enabled = false;
        this._zoneCounter = 0;
      }
      ZoneContextManager2.prototype._activeContextFromZone = function(activeZone) {
        return activeZone && activeZone.get(ZONE_CONTEXT_KEY) || ROOT_CONTEXT;
      };
      ZoneContextManager2.prototype._bindFunction = function(context2, target) {
        var manager = this;
        var contextWrapper = function() {
          var _this = this;
          var args = [];
          for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
          }
          return manager.with(context2, function() {
            return target.apply(_this, args);
          });
        };
        Object.defineProperty(contextWrapper, "length", {
          enumerable: false,
          configurable: true,
          writable: false,
          value: target.length
        });
        return contextWrapper;
      };
      ZoneContextManager2.prototype._bindListener = function(context2, obj) {
        var target = obj;
        if (target.__ot_listeners !== void 0) {
          return obj;
        }
        target.__ot_listeners = {};
        if (typeof target.addEventListener === "function") {
          target.addEventListener = this._patchAddEventListener(target, target.addEventListener, context2);
        }
        if (typeof target.removeEventListener === "function") {
          target.removeEventListener = this._patchRemoveEventListener(target, target.removeEventListener);
        }
        return obj;
      };
      ZoneContextManager2.prototype._createZoneName = function() {
        this._zoneCounter++;
        var random = Math.random();
        return this._zoneCounter + "-" + random;
      };
      ZoneContextManager2.prototype._createZone = function(zoneName, context2) {
        var _a2;
        return Zone.current.fork({
          name: zoneName,
          properties: (_a2 = {}, _a2[ZONE_CONTEXT_KEY] = context2, _a2)
        });
      };
      ZoneContextManager2.prototype._getActiveZone = function() {
        return Zone.current;
      };
      ZoneContextManager2.prototype._patchAddEventListener = function(target, original, context2) {
        var contextManager = this;
        return function(event, listener, opts) {
          if (target.__ot_listeners === void 0) {
            target.__ot_listeners = {};
          }
          var listeners = target.__ot_listeners[event];
          if (listeners === void 0) {
            listeners = /* @__PURE__ */ new WeakMap();
            target.__ot_listeners[event] = listeners;
          }
          var patchedListener = contextManager.bind(context2, listener);
          listeners.set(listener, patchedListener);
          return original.call(this, event, patchedListener, opts);
        };
      };
      ZoneContextManager2.prototype._patchRemoveEventListener = function(target, original) {
        return function(event, listener) {
          if (target.__ot_listeners === void 0 || target.__ot_listeners[event] === void 0) {
            return original.call(this, event, listener);
          }
          var events = target.__ot_listeners[event];
          var patchedListener = events.get(listener);
          events.delete(listener);
          return original.call(this, event, patchedListener || listener);
        };
      };
      ZoneContextManager2.prototype.active = function() {
        if (!this._enabled) {
          return ROOT_CONTEXT;
        }
        var activeZone = this._getActiveZone();
        var active = this._activeContextFromZone(activeZone);
        if (active) {
          return active;
        }
        return ROOT_CONTEXT;
      };
      ZoneContextManager2.prototype.bind = function(context2, target) {
        if (context2 === void 0) {
          context2 = this.active();
        }
        if (typeof target === "function") {
          return this._bindFunction(context2, target);
        } else if (isListenerObject(target)) {
          this._bindListener(context2, target);
        }
        return target;
      };
      ZoneContextManager2.prototype.disable = function() {
        this._enabled = false;
        return this;
      };
      ZoneContextManager2.prototype.enable = function() {
        this._enabled = true;
        return this;
      };
      ZoneContextManager2.prototype.with = function(context2, fn, thisArg) {
        var args = [];
        for (var _i = 3; _i < arguments.length; _i++) {
          args[_i - 3] = arguments[_i];
        }
        var zoneName = this._createZoneName();
        var newZone = this._createZone(zoneName, context2);
        return newZone.run(fn, thisArg, args);
      };
      return ZoneContextManager2;
    }()
  );

  // node_modules/zone.js/fesm2015/zone.js
  var Zone$1 = function(global2) {
    const performance2 = global2["performance"];
    function mark(name) {
      performance2 && performance2["mark"] && performance2["mark"](name);
    }
    function performanceMeasure(name, label) {
      performance2 && performance2["measure"] && performance2["measure"](name, label);
    }
    mark("Zone");
    const symbolPrefix = global2["__Zone_symbol_prefix"] || "__zone_symbol__";
    function __symbol__(name) {
      return symbolPrefix + name;
    }
    const checkDuplicate = global2[__symbol__("forceDuplicateZoneCheck")] === true;
    if (global2["Zone"]) {
      if (checkDuplicate || typeof global2["Zone"].__symbol__ !== "function") {
        throw new Error("Zone already loaded.");
      } else {
        return global2["Zone"];
      }
    }
    class Zone2 {
      constructor(parent, zoneSpec) {
        this._parent = parent;
        this._name = zoneSpec ? zoneSpec.name || "unnamed" : "<root>";
        this._properties = zoneSpec && zoneSpec.properties || {};
        this._zoneDelegate = new ZoneDelegate(this, this._parent && this._parent._zoneDelegate, zoneSpec);
      }
      static assertZonePatched() {
        if (global2["Promise"] !== patches["ZoneAwarePromise"]) {
          throw new Error("Zone.js has detected that ZoneAwarePromise `(window|global).Promise` has been overwritten.\nMost likely cause is that a Promise polyfill has been loaded after Zone.js (Polyfilling Promise api is not necessary when zone.js is loaded. If you must load one, do so before loading zone.js.)");
        }
      }
      static get root() {
        let zone = Zone2.current;
        while (zone.parent) {
          zone = zone.parent;
        }
        return zone;
      }
      static get current() {
        return _currentZoneFrame.zone;
      }
      static get currentTask() {
        return _currentTask;
      }
      // tslint:disable-next-line:require-internal-with-underscore
      static __load_patch(name, fn, ignoreDuplicate = false) {
        if (patches.hasOwnProperty(name)) {
          if (!ignoreDuplicate && checkDuplicate) {
            throw Error("Already loaded patch: " + name);
          }
        } else if (!global2["__Zone_disable_" + name]) {
          const perfName = "Zone:" + name;
          mark(perfName);
          patches[name] = fn(global2, Zone2, _api);
          performanceMeasure(perfName, perfName);
        }
      }
      get parent() {
        return this._parent;
      }
      get name() {
        return this._name;
      }
      get(key) {
        const zone = this.getZoneWith(key);
        if (zone)
          return zone._properties[key];
      }
      getZoneWith(key) {
        let current = this;
        while (current) {
          if (current._properties.hasOwnProperty(key)) {
            return current;
          }
          current = current._parent;
        }
        return null;
      }
      fork(zoneSpec) {
        if (!zoneSpec)
          throw new Error("ZoneSpec required!");
        return this._zoneDelegate.fork(this, zoneSpec);
      }
      wrap(callback, source) {
        if (typeof callback !== "function") {
          throw new Error("Expecting function got: " + callback);
        }
        const _callback = this._zoneDelegate.intercept(this, callback, source);
        const zone = this;
        return function() {
          return zone.runGuarded(_callback, this, arguments, source);
        };
      }
      run(callback, applyThis, applyArgs, source) {
        _currentZoneFrame = { parent: _currentZoneFrame, zone: this };
        try {
          return this._zoneDelegate.invoke(this, callback, applyThis, applyArgs, source);
        } finally {
          _currentZoneFrame = _currentZoneFrame.parent;
        }
      }
      runGuarded(callback, applyThis = null, applyArgs, source) {
        _currentZoneFrame = { parent: _currentZoneFrame, zone: this };
        try {
          try {
            return this._zoneDelegate.invoke(this, callback, applyThis, applyArgs, source);
          } catch (error) {
            if (this._zoneDelegate.handleError(this, error)) {
              throw error;
            }
          }
        } finally {
          _currentZoneFrame = _currentZoneFrame.parent;
        }
      }
      runTask(task, applyThis, applyArgs) {
        if (task.zone != this) {
          throw new Error("A task can only be run in the zone of creation! (Creation: " + (task.zone || NO_ZONE).name + "; Execution: " + this.name + ")");
        }
        if (task.state === notScheduled && (task.type === eventTask || task.type === macroTask)) {
          return;
        }
        const reEntryGuard = task.state != running;
        reEntryGuard && task._transitionTo(running, scheduled);
        task.runCount++;
        const previousTask = _currentTask;
        _currentTask = task;
        _currentZoneFrame = { parent: _currentZoneFrame, zone: this };
        try {
          if (task.type == macroTask && task.data && !task.data.isPeriodic) {
            task.cancelFn = void 0;
          }
          try {
            return this._zoneDelegate.invokeTask(this, task, applyThis, applyArgs);
          } catch (error) {
            if (this._zoneDelegate.handleError(this, error)) {
              throw error;
            }
          }
        } finally {
          if (task.state !== notScheduled && task.state !== unknown) {
            if (task.type == eventTask || task.data && task.data.isPeriodic) {
              reEntryGuard && task._transitionTo(scheduled, running);
            } else {
              task.runCount = 0;
              this._updateTaskCount(task, -1);
              reEntryGuard && task._transitionTo(notScheduled, running, notScheduled);
            }
          }
          _currentZoneFrame = _currentZoneFrame.parent;
          _currentTask = previousTask;
        }
      }
      scheduleTask(task) {
        if (task.zone && task.zone !== this) {
          let newZone = this;
          while (newZone) {
            if (newZone === task.zone) {
              throw Error(`can not reschedule task to ${this.name} which is descendants of the original zone ${task.zone.name}`);
            }
            newZone = newZone.parent;
          }
        }
        task._transitionTo(scheduling, notScheduled);
        const zoneDelegates = [];
        task._zoneDelegates = zoneDelegates;
        task._zone = this;
        try {
          task = this._zoneDelegate.scheduleTask(this, task);
        } catch (err) {
          task._transitionTo(unknown, scheduling, notScheduled);
          this._zoneDelegate.handleError(this, err);
          throw err;
        }
        if (task._zoneDelegates === zoneDelegates) {
          this._updateTaskCount(task, 1);
        }
        if (task.state == scheduling) {
          task._transitionTo(scheduled, scheduling);
        }
        return task;
      }
      scheduleMicroTask(source, callback, data, customSchedule) {
        return this.scheduleTask(new ZoneTask(microTask, source, callback, data, customSchedule, void 0));
      }
      scheduleMacroTask(source, callback, data, customSchedule, customCancel) {
        return this.scheduleTask(new ZoneTask(macroTask, source, callback, data, customSchedule, customCancel));
      }
      scheduleEventTask(source, callback, data, customSchedule, customCancel) {
        return this.scheduleTask(new ZoneTask(eventTask, source, callback, data, customSchedule, customCancel));
      }
      cancelTask(task) {
        if (task.zone != this)
          throw new Error("A task can only be cancelled in the zone of creation! (Creation: " + (task.zone || NO_ZONE).name + "; Execution: " + this.name + ")");
        task._transitionTo(canceling, scheduled, running);
        try {
          this._zoneDelegate.cancelTask(this, task);
        } catch (err) {
          task._transitionTo(unknown, canceling);
          this._zoneDelegate.handleError(this, err);
          throw err;
        }
        this._updateTaskCount(task, -1);
        task._transitionTo(notScheduled, canceling);
        task.runCount = 0;
        return task;
      }
      _updateTaskCount(task, count) {
        const zoneDelegates = task._zoneDelegates;
        if (count == -1) {
          task._zoneDelegates = null;
        }
        for (let i2 = 0; i2 < zoneDelegates.length; i2++) {
          zoneDelegates[i2]._updateTaskCount(task.type, count);
        }
      }
    }
    Zone2.__symbol__ = __symbol__;
    const DELEGATE_ZS = {
      name: "",
      onHasTask: (delegate, _2, target, hasTaskState) => delegate.hasTask(target, hasTaskState),
      onScheduleTask: (delegate, _2, target, task) => delegate.scheduleTask(target, task),
      onInvokeTask: (delegate, _2, target, task, applyThis, applyArgs) => delegate.invokeTask(target, task, applyThis, applyArgs),
      onCancelTask: (delegate, _2, target, task) => delegate.cancelTask(target, task)
    };
    class ZoneDelegate {
      constructor(zone, parentDelegate, zoneSpec) {
        this._taskCounts = { "microTask": 0, "macroTask": 0, "eventTask": 0 };
        this.zone = zone;
        this._parentDelegate = parentDelegate;
        this._forkZS = zoneSpec && (zoneSpec && zoneSpec.onFork ? zoneSpec : parentDelegate._forkZS);
        this._forkDlgt = zoneSpec && (zoneSpec.onFork ? parentDelegate : parentDelegate._forkDlgt);
        this._forkCurrZone = zoneSpec && (zoneSpec.onFork ? this.zone : parentDelegate._forkCurrZone);
        this._interceptZS = zoneSpec && (zoneSpec.onIntercept ? zoneSpec : parentDelegate._interceptZS);
        this._interceptDlgt = zoneSpec && (zoneSpec.onIntercept ? parentDelegate : parentDelegate._interceptDlgt);
        this._interceptCurrZone = zoneSpec && (zoneSpec.onIntercept ? this.zone : parentDelegate._interceptCurrZone);
        this._invokeZS = zoneSpec && (zoneSpec.onInvoke ? zoneSpec : parentDelegate._invokeZS);
        this._invokeDlgt = zoneSpec && (zoneSpec.onInvoke ? parentDelegate : parentDelegate._invokeDlgt);
        this._invokeCurrZone = zoneSpec && (zoneSpec.onInvoke ? this.zone : parentDelegate._invokeCurrZone);
        this._handleErrorZS = zoneSpec && (zoneSpec.onHandleError ? zoneSpec : parentDelegate._handleErrorZS);
        this._handleErrorDlgt = zoneSpec && (zoneSpec.onHandleError ? parentDelegate : parentDelegate._handleErrorDlgt);
        this._handleErrorCurrZone = zoneSpec && (zoneSpec.onHandleError ? this.zone : parentDelegate._handleErrorCurrZone);
        this._scheduleTaskZS = zoneSpec && (zoneSpec.onScheduleTask ? zoneSpec : parentDelegate._scheduleTaskZS);
        this._scheduleTaskDlgt = zoneSpec && (zoneSpec.onScheduleTask ? parentDelegate : parentDelegate._scheduleTaskDlgt);
        this._scheduleTaskCurrZone = zoneSpec && (zoneSpec.onScheduleTask ? this.zone : parentDelegate._scheduleTaskCurrZone);
        this._invokeTaskZS = zoneSpec && (zoneSpec.onInvokeTask ? zoneSpec : parentDelegate._invokeTaskZS);
        this._invokeTaskDlgt = zoneSpec && (zoneSpec.onInvokeTask ? parentDelegate : parentDelegate._invokeTaskDlgt);
        this._invokeTaskCurrZone = zoneSpec && (zoneSpec.onInvokeTask ? this.zone : parentDelegate._invokeTaskCurrZone);
        this._cancelTaskZS = zoneSpec && (zoneSpec.onCancelTask ? zoneSpec : parentDelegate._cancelTaskZS);
        this._cancelTaskDlgt = zoneSpec && (zoneSpec.onCancelTask ? parentDelegate : parentDelegate._cancelTaskDlgt);
        this._cancelTaskCurrZone = zoneSpec && (zoneSpec.onCancelTask ? this.zone : parentDelegate._cancelTaskCurrZone);
        this._hasTaskZS = null;
        this._hasTaskDlgt = null;
        this._hasTaskDlgtOwner = null;
        this._hasTaskCurrZone = null;
        const zoneSpecHasTask = zoneSpec && zoneSpec.onHasTask;
        const parentHasTask = parentDelegate && parentDelegate._hasTaskZS;
        if (zoneSpecHasTask || parentHasTask) {
          this._hasTaskZS = zoneSpecHasTask ? zoneSpec : DELEGATE_ZS;
          this._hasTaskDlgt = parentDelegate;
          this._hasTaskDlgtOwner = this;
          this._hasTaskCurrZone = zone;
          if (!zoneSpec.onScheduleTask) {
            this._scheduleTaskZS = DELEGATE_ZS;
            this._scheduleTaskDlgt = parentDelegate;
            this._scheduleTaskCurrZone = this.zone;
          }
          if (!zoneSpec.onInvokeTask) {
            this._invokeTaskZS = DELEGATE_ZS;
            this._invokeTaskDlgt = parentDelegate;
            this._invokeTaskCurrZone = this.zone;
          }
          if (!zoneSpec.onCancelTask) {
            this._cancelTaskZS = DELEGATE_ZS;
            this._cancelTaskDlgt = parentDelegate;
            this._cancelTaskCurrZone = this.zone;
          }
        }
      }
      fork(targetZone, zoneSpec) {
        return this._forkZS ? this._forkZS.onFork(this._forkDlgt, this.zone, targetZone, zoneSpec) : new Zone2(targetZone, zoneSpec);
      }
      intercept(targetZone, callback, source) {
        return this._interceptZS ? this._interceptZS.onIntercept(this._interceptDlgt, this._interceptCurrZone, targetZone, callback, source) : callback;
      }
      invoke(targetZone, callback, applyThis, applyArgs, source) {
        return this._invokeZS ? this._invokeZS.onInvoke(this._invokeDlgt, this._invokeCurrZone, targetZone, callback, applyThis, applyArgs, source) : callback.apply(applyThis, applyArgs);
      }
      handleError(targetZone, error) {
        return this._handleErrorZS ? this._handleErrorZS.onHandleError(this._handleErrorDlgt, this._handleErrorCurrZone, targetZone, error) : true;
      }
      scheduleTask(targetZone, task) {
        let returnTask = task;
        if (this._scheduleTaskZS) {
          if (this._hasTaskZS) {
            returnTask._zoneDelegates.push(this._hasTaskDlgtOwner);
          }
          returnTask = this._scheduleTaskZS.onScheduleTask(this._scheduleTaskDlgt, this._scheduleTaskCurrZone, targetZone, task);
          if (!returnTask)
            returnTask = task;
        } else {
          if (task.scheduleFn) {
            task.scheduleFn(task);
          } else if (task.type == microTask) {
            scheduleMicroTask(task);
          } else {
            throw new Error("Task is missing scheduleFn.");
          }
        }
        return returnTask;
      }
      invokeTask(targetZone, task, applyThis, applyArgs) {
        return this._invokeTaskZS ? this._invokeTaskZS.onInvokeTask(this._invokeTaskDlgt, this._invokeTaskCurrZone, targetZone, task, applyThis, applyArgs) : task.callback.apply(applyThis, applyArgs);
      }
      cancelTask(targetZone, task) {
        let value;
        if (this._cancelTaskZS) {
          value = this._cancelTaskZS.onCancelTask(this._cancelTaskDlgt, this._cancelTaskCurrZone, targetZone, task);
        } else {
          if (!task.cancelFn) {
            throw Error("Task is not cancelable");
          }
          value = task.cancelFn(task);
        }
        return value;
      }
      hasTask(targetZone, isEmpty) {
        try {
          this._hasTaskZS && this._hasTaskZS.onHasTask(this._hasTaskDlgt, this._hasTaskCurrZone, targetZone, isEmpty);
        } catch (err) {
          this.handleError(targetZone, err);
        }
      }
      // tslint:disable-next-line:require-internal-with-underscore
      _updateTaskCount(type, count) {
        const counts = this._taskCounts;
        const prev = counts[type];
        const next = counts[type] = prev + count;
        if (next < 0) {
          throw new Error("More tasks executed then were scheduled.");
        }
        if (prev == 0 || next == 0) {
          const isEmpty = {
            microTask: counts["microTask"] > 0,
            macroTask: counts["macroTask"] > 0,
            eventTask: counts["eventTask"] > 0,
            change: type
          };
          this.hasTask(this.zone, isEmpty);
        }
      }
    }
    class ZoneTask {
      constructor(type, source, callback, options, scheduleFn, cancelFn) {
        this._zone = null;
        this.runCount = 0;
        this._zoneDelegates = null;
        this._state = "notScheduled";
        this.type = type;
        this.source = source;
        this.data = options;
        this.scheduleFn = scheduleFn;
        this.cancelFn = cancelFn;
        if (!callback) {
          throw new Error("callback is not defined");
        }
        this.callback = callback;
        const self2 = this;
        if (type === eventTask && options && options.useG) {
          this.invoke = ZoneTask.invokeTask;
        } else {
          this.invoke = function() {
            return ZoneTask.invokeTask.call(global2, self2, this, arguments);
          };
        }
      }
      static invokeTask(task, target, args) {
        if (!task) {
          task = this;
        }
        _numberOfNestedTaskFrames++;
        try {
          task.runCount++;
          return task.zone.runTask(task, target, args);
        } finally {
          if (_numberOfNestedTaskFrames == 1) {
            drainMicroTaskQueue();
          }
          _numberOfNestedTaskFrames--;
        }
      }
      get zone() {
        return this._zone;
      }
      get state() {
        return this._state;
      }
      cancelScheduleRequest() {
        this._transitionTo(notScheduled, scheduling);
      }
      // tslint:disable-next-line:require-internal-with-underscore
      _transitionTo(toState, fromState1, fromState2) {
        if (this._state === fromState1 || this._state === fromState2) {
          this._state = toState;
          if (toState == notScheduled) {
            this._zoneDelegates = null;
          }
        } else {
          throw new Error(`${this.type} '${this.source}': can not transition to '${toState}', expecting state '${fromState1}'${fromState2 ? " or '" + fromState2 + "'" : ""}, was '${this._state}'.`);
        }
      }
      toString() {
        if (this.data && typeof this.data.handleId !== "undefined") {
          return this.data.handleId.toString();
        } else {
          return Object.prototype.toString.call(this);
        }
      }
      // add toJSON method to prevent cyclic error when
      // call JSON.stringify(zoneTask)
      toJSON() {
        return {
          type: this.type,
          state: this.state,
          source: this.source,
          zone: this.zone.name,
          runCount: this.runCount
        };
      }
    }
    const symbolSetTimeout = __symbol__("setTimeout");
    const symbolPromise = __symbol__("Promise");
    const symbolThen = __symbol__("then");
    let _microTaskQueue = [];
    let _isDrainingMicrotaskQueue = false;
    let nativeMicroTaskQueuePromise;
    function scheduleMicroTask(task) {
      if (_numberOfNestedTaskFrames === 0 && _microTaskQueue.length === 0) {
        if (!nativeMicroTaskQueuePromise) {
          if (global2[symbolPromise]) {
            nativeMicroTaskQueuePromise = global2[symbolPromise].resolve(0);
          }
        }
        if (nativeMicroTaskQueuePromise) {
          let nativeThen = nativeMicroTaskQueuePromise[symbolThen];
          if (!nativeThen) {
            nativeThen = nativeMicroTaskQueuePromise["then"];
          }
          nativeThen.call(nativeMicroTaskQueuePromise, drainMicroTaskQueue);
        } else {
          global2[symbolSetTimeout](drainMicroTaskQueue, 0);
        }
      }
      task && _microTaskQueue.push(task);
    }
    function drainMicroTaskQueue() {
      if (!_isDrainingMicrotaskQueue) {
        _isDrainingMicrotaskQueue = true;
        while (_microTaskQueue.length) {
          const queue = _microTaskQueue;
          _microTaskQueue = [];
          for (let i2 = 0; i2 < queue.length; i2++) {
            const task = queue[i2];
            try {
              task.zone.runTask(task, null, null);
            } catch (error) {
              _api.onUnhandledError(error);
            }
          }
        }
        _api.microtaskDrainDone();
        _isDrainingMicrotaskQueue = false;
      }
    }
    const NO_ZONE = { name: "NO ZONE" };
    const notScheduled = "notScheduled", scheduling = "scheduling", scheduled = "scheduled", running = "running", canceling = "canceling", unknown = "unknown";
    const microTask = "microTask", macroTask = "macroTask", eventTask = "eventTask";
    const patches = {};
    const _api = {
      symbol: __symbol__,
      currentZoneFrame: () => _currentZoneFrame,
      onUnhandledError: noop,
      microtaskDrainDone: noop,
      scheduleMicroTask,
      showUncaughtError: () => !Zone2[__symbol__("ignoreConsoleErrorUncaughtError")],
      patchEventTarget: () => [],
      patchOnProperties: noop,
      patchMethod: () => noop,
      bindArguments: () => [],
      patchThen: () => noop,
      patchMacroTask: () => noop,
      patchEventPrototype: () => noop,
      isIEOrEdge: () => false,
      getGlobalObjects: () => void 0,
      ObjectDefineProperty: () => noop,
      ObjectGetOwnPropertyDescriptor: () => void 0,
      ObjectCreate: () => void 0,
      ArraySlice: () => [],
      patchClass: () => noop,
      wrapWithCurrentZone: () => noop,
      filterProperties: () => [],
      attachOriginToPatched: () => noop,
      _redefineProperty: () => noop,
      patchCallbacks: () => noop
    };
    let _currentZoneFrame = { parent: null, zone: new Zone2(null, null) };
    let _currentTask = null;
    let _numberOfNestedTaskFrames = 0;
    function noop() {
    }
    performanceMeasure("Zone", "Zone");
    return global2["Zone"] = Zone2;
  }(typeof window !== "undefined" && window || typeof self !== "undefined" && self || global);
  var ObjectGetOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  var ObjectDefineProperty = Object.defineProperty;
  var ObjectGetPrototypeOf = Object.getPrototypeOf;
  var ObjectCreate = Object.create;
  var ArraySlice = Array.prototype.slice;
  var ADD_EVENT_LISTENER_STR = "addEventListener";
  var REMOVE_EVENT_LISTENER_STR = "removeEventListener";
  var ZONE_SYMBOL_ADD_EVENT_LISTENER = Zone.__symbol__(ADD_EVENT_LISTENER_STR);
  var ZONE_SYMBOL_REMOVE_EVENT_LISTENER = Zone.__symbol__(REMOVE_EVENT_LISTENER_STR);
  var TRUE_STR = "true";
  var FALSE_STR = "false";
  var ZONE_SYMBOL_PREFIX = Zone.__symbol__("");
  function wrapWithCurrentZone(callback, source) {
    return Zone.current.wrap(callback, source);
  }
  function scheduleMacroTaskWithCurrentZone(source, callback, data, customSchedule, customCancel) {
    return Zone.current.scheduleMacroTask(source, callback, data, customSchedule, customCancel);
  }
  var zoneSymbol = Zone.__symbol__;
  var isWindowExists = typeof window !== "undefined";
  var internalWindow = isWindowExists ? window : void 0;
  var _global2 = isWindowExists && internalWindow || typeof self === "object" && self || global;
  var REMOVE_ATTRIBUTE = "removeAttribute";
  var NULL_ON_PROP_VALUE = [null];
  function bindArguments(args, source) {
    for (let i2 = args.length - 1; i2 >= 0; i2--) {
      if (typeof args[i2] === "function") {
        args[i2] = wrapWithCurrentZone(args[i2], source + "_" + i2);
      }
    }
    return args;
  }
  function patchPrototype(prototype, fnNames) {
    const source = prototype.constructor["name"];
    for (let i2 = 0; i2 < fnNames.length; i2++) {
      const name = fnNames[i2];
      const delegate = prototype[name];
      if (delegate) {
        const prototypeDesc = ObjectGetOwnPropertyDescriptor(prototype, name);
        if (!isPropertyWritable(prototypeDesc)) {
          continue;
        }
        prototype[name] = ((delegate2) => {
          const patched = function() {
            return delegate2.apply(this, bindArguments(arguments, source + "." + name));
          };
          attachOriginToPatched(patched, delegate2);
          return patched;
        })(delegate);
      }
    }
  }
  function isPropertyWritable(propertyDesc) {
    if (!propertyDesc) {
      return true;
    }
    if (propertyDesc.writable === false) {
      return false;
    }
    return !(typeof propertyDesc.get === "function" && typeof propertyDesc.set === "undefined");
  }
  var isWebWorker = typeof WorkerGlobalScope !== "undefined" && self instanceof WorkerGlobalScope;
  var isNode = !("nw" in _global2) && typeof _global2.process !== "undefined" && {}.toString.call(_global2.process) === "[object process]";
  var isBrowser = !isNode && !isWebWorker && !!(isWindowExists && internalWindow["HTMLElement"]);
  var isMix = typeof _global2.process !== "undefined" && {}.toString.call(_global2.process) === "[object process]" && !isWebWorker && !!(isWindowExists && internalWindow["HTMLElement"]);
  var zoneSymbolEventNames = {};
  var wrapFn = function(event) {
    event = event || _global2.event;
    if (!event) {
      return;
    }
    let eventNameSymbol = zoneSymbolEventNames[event.type];
    if (!eventNameSymbol) {
      eventNameSymbol = zoneSymbolEventNames[event.type] = zoneSymbol("ON_PROPERTY" + event.type);
    }
    const target = this || event.target || _global2;
    const listener = target[eventNameSymbol];
    let result;
    if (isBrowser && target === internalWindow && event.type === "error") {
      const errorEvent = event;
      result = listener && listener.call(this, errorEvent.message, errorEvent.filename, errorEvent.lineno, errorEvent.colno, errorEvent.error);
      if (result === true) {
        event.preventDefault();
      }
    } else {
      result = listener && listener.apply(this, arguments);
      if (result != void 0 && !result) {
        event.preventDefault();
      }
    }
    return result;
  };
  function patchProperty(obj, prop, prototype) {
    let desc = ObjectGetOwnPropertyDescriptor(obj, prop);
    if (!desc && prototype) {
      const prototypeDesc = ObjectGetOwnPropertyDescriptor(prototype, prop);
      if (prototypeDesc) {
        desc = { enumerable: true, configurable: true };
      }
    }
    if (!desc || !desc.configurable) {
      return;
    }
    const onPropPatchedSymbol = zoneSymbol("on" + prop + "patched");
    if (obj.hasOwnProperty(onPropPatchedSymbol) && obj[onPropPatchedSymbol]) {
      return;
    }
    delete desc.writable;
    delete desc.value;
    const originalDescGet = desc.get;
    const originalDescSet = desc.set;
    const eventName = prop.substr(2);
    let eventNameSymbol = zoneSymbolEventNames[eventName];
    if (!eventNameSymbol) {
      eventNameSymbol = zoneSymbolEventNames[eventName] = zoneSymbol("ON_PROPERTY" + eventName);
    }
    desc.set = function(newValue) {
      let target = this;
      if (!target && obj === _global2) {
        target = _global2;
      }
      if (!target) {
        return;
      }
      let previousValue = target[eventNameSymbol];
      if (previousValue) {
        target.removeEventListener(eventName, wrapFn);
      }
      if (originalDescSet) {
        originalDescSet.apply(target, NULL_ON_PROP_VALUE);
      }
      if (typeof newValue === "function") {
        target[eventNameSymbol] = newValue;
        target.addEventListener(eventName, wrapFn, false);
      } else {
        target[eventNameSymbol] = null;
      }
    };
    desc.get = function() {
      let target = this;
      if (!target && obj === _global2) {
        target = _global2;
      }
      if (!target) {
        return null;
      }
      const listener = target[eventNameSymbol];
      if (listener) {
        return listener;
      } else if (originalDescGet) {
        let value = originalDescGet && originalDescGet.call(this);
        if (value) {
          desc.set.call(this, value);
          if (typeof target[REMOVE_ATTRIBUTE] === "function") {
            target.removeAttribute(prop);
          }
          return value;
        }
      }
      return null;
    };
    ObjectDefineProperty(obj, prop, desc);
    obj[onPropPatchedSymbol] = true;
  }
  function patchOnProperties(obj, properties, prototype) {
    if (properties) {
      for (let i2 = 0; i2 < properties.length; i2++) {
        patchProperty(obj, "on" + properties[i2], prototype);
      }
    } else {
      const onProperties = [];
      for (const prop in obj) {
        if (prop.substr(0, 2) == "on") {
          onProperties.push(prop);
        }
      }
      for (let j2 = 0; j2 < onProperties.length; j2++) {
        patchProperty(obj, onProperties[j2], prototype);
      }
    }
  }
  var originalInstanceKey = zoneSymbol("originalInstance");
  function patchClass(className) {
    const OriginalClass = _global2[className];
    if (!OriginalClass)
      return;
    _global2[zoneSymbol(className)] = OriginalClass;
    _global2[className] = function() {
      const a2 = bindArguments(arguments, className);
      switch (a2.length) {
        case 0:
          this[originalInstanceKey] = new OriginalClass();
          break;
        case 1:
          this[originalInstanceKey] = new OriginalClass(a2[0]);
          break;
        case 2:
          this[originalInstanceKey] = new OriginalClass(a2[0], a2[1]);
          break;
        case 3:
          this[originalInstanceKey] = new OriginalClass(a2[0], a2[1], a2[2]);
          break;
        case 4:
          this[originalInstanceKey] = new OriginalClass(a2[0], a2[1], a2[2], a2[3]);
          break;
        default:
          throw new Error("Arg list too long.");
      }
    };
    attachOriginToPatched(_global2[className], OriginalClass);
    const instance = new OriginalClass(function() {
    });
    let prop;
    for (prop in instance) {
      if (className === "XMLHttpRequest" && prop === "responseBlob")
        continue;
      (function(prop2) {
        if (typeof instance[prop2] === "function") {
          _global2[className].prototype[prop2] = function() {
            return this[originalInstanceKey][prop2].apply(this[originalInstanceKey], arguments);
          };
        } else {
          ObjectDefineProperty(_global2[className].prototype, prop2, {
            set: function(fn) {
              if (typeof fn === "function") {
                this[originalInstanceKey][prop2] = wrapWithCurrentZone(fn, className + "." + prop2);
                attachOriginToPatched(this[originalInstanceKey][prop2], fn);
              } else {
                this[originalInstanceKey][prop2] = fn;
              }
            },
            get: function() {
              return this[originalInstanceKey][prop2];
            }
          });
        }
      })(prop);
    }
    for (prop in OriginalClass) {
      if (prop !== "prototype" && OriginalClass.hasOwnProperty(prop)) {
        _global2[className][prop] = OriginalClass[prop];
      }
    }
  }
  function patchMethod(target, name, patchFn) {
    let proto = target;
    while (proto && !proto.hasOwnProperty(name)) {
      proto = ObjectGetPrototypeOf(proto);
    }
    if (!proto && target[name]) {
      proto = target;
    }
    const delegateName = zoneSymbol(name);
    let delegate = null;
    if (proto && (!(delegate = proto[delegateName]) || !proto.hasOwnProperty(delegateName))) {
      delegate = proto[delegateName] = proto[name];
      const desc = proto && ObjectGetOwnPropertyDescriptor(proto, name);
      if (isPropertyWritable(desc)) {
        const patchDelegate = patchFn(delegate, delegateName, name);
        proto[name] = function() {
          return patchDelegate(this, arguments);
        };
        attachOriginToPatched(proto[name], delegate);
      }
    }
    return delegate;
  }
  function patchMacroTask(obj, funcName, metaCreator) {
    let setNative = null;
    function scheduleTask(task) {
      const data = task.data;
      data.args[data.cbIdx] = function() {
        task.invoke.apply(this, arguments);
      };
      setNative.apply(data.target, data.args);
      return task;
    }
    setNative = patchMethod(obj, funcName, (delegate) => function(self2, args) {
      const meta = metaCreator(self2, args);
      if (meta.cbIdx >= 0 && typeof args[meta.cbIdx] === "function") {
        return scheduleMacroTaskWithCurrentZone(meta.name, args[meta.cbIdx], meta, scheduleTask);
      } else {
        return delegate.apply(self2, args);
      }
    });
  }
  function attachOriginToPatched(patched, original) {
    patched[zoneSymbol("OriginalDelegate")] = original;
  }
  var isDetectedIEOrEdge = false;
  var ieOrEdge = false;
  function isIE() {
    try {
      const ua = internalWindow.navigator.userAgent;
      if (ua.indexOf("MSIE ") !== -1 || ua.indexOf("Trident/") !== -1) {
        return true;
      }
    } catch (error) {
    }
    return false;
  }
  function isIEOrEdge() {
    if (isDetectedIEOrEdge) {
      return ieOrEdge;
    }
    isDetectedIEOrEdge = true;
    try {
      const ua = internalWindow.navigator.userAgent;
      if (ua.indexOf("MSIE ") !== -1 || ua.indexOf("Trident/") !== -1 || ua.indexOf("Edge/") !== -1) {
        ieOrEdge = true;
      }
    } catch (error) {
    }
    return ieOrEdge;
  }
  Zone.__load_patch("ZoneAwarePromise", (global2, Zone2, api) => {
    const ObjectGetOwnPropertyDescriptor2 = Object.getOwnPropertyDescriptor;
    const ObjectDefineProperty2 = Object.defineProperty;
    function readableObjectToString(obj) {
      if (obj && obj.toString === Object.prototype.toString) {
        const className = obj.constructor && obj.constructor.name;
        return (className ? className : "") + ": " + JSON.stringify(obj);
      }
      return obj ? obj.toString() : Object.prototype.toString.call(obj);
    }
    const __symbol__ = api.symbol;
    const _uncaughtPromiseErrors = [];
    const isDisableWrappingUncaughtPromiseRejection = global2[__symbol__("DISABLE_WRAPPING_UNCAUGHT_PROMISE_REJECTION")] === true;
    const symbolPromise = __symbol__("Promise");
    const symbolThen = __symbol__("then");
    const creationTrace = "__creationTrace__";
    api.onUnhandledError = (e2) => {
      if (api.showUncaughtError()) {
        const rejection = e2 && e2.rejection;
        if (rejection) {
          console.error("Unhandled Promise rejection:", rejection instanceof Error ? rejection.message : rejection, "; Zone:", e2.zone.name, "; Task:", e2.task && e2.task.source, "; Value:", rejection, rejection instanceof Error ? rejection.stack : void 0);
        } else {
          console.error(e2);
        }
      }
    };
    api.microtaskDrainDone = () => {
      while (_uncaughtPromiseErrors.length) {
        const uncaughtPromiseError = _uncaughtPromiseErrors.shift();
        try {
          uncaughtPromiseError.zone.runGuarded(() => {
            if (uncaughtPromiseError.throwOriginal) {
              throw uncaughtPromiseError.rejection;
            }
            throw uncaughtPromiseError;
          });
        } catch (error) {
          handleUnhandledRejection(error);
        }
      }
    };
    const UNHANDLED_PROMISE_REJECTION_HANDLER_SYMBOL = __symbol__("unhandledPromiseRejectionHandler");
    function handleUnhandledRejection(e2) {
      api.onUnhandledError(e2);
      try {
        const handler = Zone2[UNHANDLED_PROMISE_REJECTION_HANDLER_SYMBOL];
        if (typeof handler === "function") {
          handler.call(this, e2);
        }
      } catch (err) {
      }
    }
    function isThenable(value) {
      return value && value.then;
    }
    function forwardResolution(value) {
      return value;
    }
    function forwardRejection(rejection) {
      return ZoneAwarePromise.reject(rejection);
    }
    const symbolState = __symbol__("state");
    const symbolValue = __symbol__("value");
    const symbolFinally = __symbol__("finally");
    const symbolParentPromiseValue = __symbol__("parentPromiseValue");
    const symbolParentPromiseState = __symbol__("parentPromiseState");
    const source = "Promise.then";
    const UNRESOLVED = null;
    const RESOLVED = true;
    const REJECTED = false;
    const REJECTED_NO_CATCH = 0;
    function makeResolver(promise, state) {
      return (v2) => {
        try {
          resolvePromise(promise, state, v2);
        } catch (err) {
          resolvePromise(promise, false, err);
        }
      };
    }
    const once = function() {
      let wasCalled = false;
      return function wrapper(wrappedFunction) {
        return function() {
          if (wasCalled) {
            return;
          }
          wasCalled = true;
          wrappedFunction.apply(null, arguments);
        };
      };
    };
    const TYPE_ERROR = "Promise resolved with itself";
    const CURRENT_TASK_TRACE_SYMBOL = __symbol__("currentTaskTrace");
    function resolvePromise(promise, state, value) {
      const onceWrapper = once();
      if (promise === value) {
        throw new TypeError(TYPE_ERROR);
      }
      if (promise[symbolState] === UNRESOLVED) {
        let then = null;
        try {
          if (typeof value === "object" || typeof value === "function") {
            then = value && value.then;
          }
        } catch (err) {
          onceWrapper(() => {
            resolvePromise(promise, false, err);
          })();
          return promise;
        }
        if (state !== REJECTED && value instanceof ZoneAwarePromise && value.hasOwnProperty(symbolState) && value.hasOwnProperty(symbolValue) && value[symbolState] !== UNRESOLVED) {
          clearRejectedNoCatch(value);
          resolvePromise(promise, value[symbolState], value[symbolValue]);
        } else if (state !== REJECTED && typeof then === "function") {
          try {
            then.call(value, onceWrapper(makeResolver(promise, state)), onceWrapper(makeResolver(promise, false)));
          } catch (err) {
            onceWrapper(() => {
              resolvePromise(promise, false, err);
            })();
          }
        } else {
          promise[symbolState] = state;
          const queue = promise[symbolValue];
          promise[symbolValue] = value;
          if (promise[symbolFinally] === symbolFinally) {
            if (state === RESOLVED) {
              promise[symbolState] = promise[symbolParentPromiseState];
              promise[symbolValue] = promise[symbolParentPromiseValue];
            }
          }
          if (state === REJECTED && value instanceof Error) {
            const trace2 = Zone2.currentTask && Zone2.currentTask.data && Zone2.currentTask.data[creationTrace];
            if (trace2) {
              ObjectDefineProperty2(value, CURRENT_TASK_TRACE_SYMBOL, { configurable: true, enumerable: false, writable: true, value: trace2 });
            }
          }
          for (let i2 = 0; i2 < queue.length; ) {
            scheduleResolveOrReject(promise, queue[i2++], queue[i2++], queue[i2++], queue[i2++]);
          }
          if (queue.length == 0 && state == REJECTED) {
            promise[symbolState] = REJECTED_NO_CATCH;
            let uncaughtPromiseError = value;
            try {
              throw new Error("Uncaught (in promise): " + readableObjectToString(value) + (value && value.stack ? "\n" + value.stack : ""));
            } catch (err) {
              uncaughtPromiseError = err;
            }
            if (isDisableWrappingUncaughtPromiseRejection) {
              uncaughtPromiseError.throwOriginal = true;
            }
            uncaughtPromiseError.rejection = value;
            uncaughtPromiseError.promise = promise;
            uncaughtPromiseError.zone = Zone2.current;
            uncaughtPromiseError.task = Zone2.currentTask;
            _uncaughtPromiseErrors.push(uncaughtPromiseError);
            api.scheduleMicroTask();
          }
        }
      }
      return promise;
    }
    const REJECTION_HANDLED_HANDLER = __symbol__("rejectionHandledHandler");
    function clearRejectedNoCatch(promise) {
      if (promise[symbolState] === REJECTED_NO_CATCH) {
        try {
          const handler = Zone2[REJECTION_HANDLED_HANDLER];
          if (handler && typeof handler === "function") {
            handler.call(this, { rejection: promise[symbolValue], promise });
          }
        } catch (err) {
        }
        promise[symbolState] = REJECTED;
        for (let i2 = 0; i2 < _uncaughtPromiseErrors.length; i2++) {
          if (promise === _uncaughtPromiseErrors[i2].promise) {
            _uncaughtPromiseErrors.splice(i2, 1);
          }
        }
      }
    }
    function scheduleResolveOrReject(promise, zone, chainPromise, onFulfilled, onRejected) {
      clearRejectedNoCatch(promise);
      const promiseState = promise[symbolState];
      const delegate = promiseState ? typeof onFulfilled === "function" ? onFulfilled : forwardResolution : typeof onRejected === "function" ? onRejected : forwardRejection;
      zone.scheduleMicroTask(source, () => {
        try {
          const parentPromiseValue = promise[symbolValue];
          const isFinallyPromise = !!chainPromise && symbolFinally === chainPromise[symbolFinally];
          if (isFinallyPromise) {
            chainPromise[symbolParentPromiseValue] = parentPromiseValue;
            chainPromise[symbolParentPromiseState] = promiseState;
          }
          const value = zone.run(delegate, void 0, isFinallyPromise && delegate !== forwardRejection && delegate !== forwardResolution ? [] : [parentPromiseValue]);
          resolvePromise(chainPromise, true, value);
        } catch (error) {
          resolvePromise(chainPromise, false, error);
        }
      }, chainPromise);
    }
    const ZONE_AWARE_PROMISE_TO_STRING = "function ZoneAwarePromise() { [native code] }";
    const noop = function() {
    };
    class ZoneAwarePromise {
      static toString() {
        return ZONE_AWARE_PROMISE_TO_STRING;
      }
      static resolve(value) {
        return resolvePromise(new this(null), RESOLVED, value);
      }
      static reject(error) {
        return resolvePromise(new this(null), REJECTED, error);
      }
      static race(values) {
        let resolve;
        let reject;
        let promise = new this((res, rej) => {
          resolve = res;
          reject = rej;
        });
        function onResolve(value) {
          resolve(value);
        }
        function onReject(error) {
          reject(error);
        }
        for (let value of values) {
          if (!isThenable(value)) {
            value = this.resolve(value);
          }
          value.then(onResolve, onReject);
        }
        return promise;
      }
      static all(values) {
        return ZoneAwarePromise.allWithCallback(values);
      }
      static allSettled(values) {
        const P2 = this && this.prototype instanceof ZoneAwarePromise ? this : ZoneAwarePromise;
        return P2.allWithCallback(values, {
          thenCallback: (value) => ({ status: "fulfilled", value }),
          errorCallback: (err) => ({ status: "rejected", reason: err })
        });
      }
      static allWithCallback(values, callback) {
        let resolve;
        let reject;
        let promise = new this((res, rej) => {
          resolve = res;
          reject = rej;
        });
        let unresolvedCount = 2;
        let valueIndex = 0;
        const resolvedValues = [];
        for (let value of values) {
          if (!isThenable(value)) {
            value = this.resolve(value);
          }
          const curValueIndex = valueIndex;
          try {
            value.then((value2) => {
              resolvedValues[curValueIndex] = callback ? callback.thenCallback(value2) : value2;
              unresolvedCount--;
              if (unresolvedCount === 0) {
                resolve(resolvedValues);
              }
            }, (err) => {
              if (!callback) {
                reject(err);
              } else {
                resolvedValues[curValueIndex] = callback.errorCallback(err);
                unresolvedCount--;
                if (unresolvedCount === 0) {
                  resolve(resolvedValues);
                }
              }
            });
          } catch (thenErr) {
            reject(thenErr);
          }
          unresolvedCount++;
          valueIndex++;
        }
        unresolvedCount -= 2;
        if (unresolvedCount === 0) {
          resolve(resolvedValues);
        }
        return promise;
      }
      constructor(executor) {
        const promise = this;
        if (!(promise instanceof ZoneAwarePromise)) {
          throw new Error("Must be an instanceof Promise.");
        }
        promise[symbolState] = UNRESOLVED;
        promise[symbolValue] = [];
        try {
          executor && executor(makeResolver(promise, RESOLVED), makeResolver(promise, REJECTED));
        } catch (error) {
          resolvePromise(promise, false, error);
        }
      }
      get [Symbol.toStringTag]() {
        return "Promise";
      }
      get [Symbol.species]() {
        return ZoneAwarePromise;
      }
      then(onFulfilled, onRejected) {
        let C2 = this.constructor[Symbol.species];
        if (!C2 || typeof C2 !== "function") {
          C2 = this.constructor || ZoneAwarePromise;
        }
        const chainPromise = new C2(noop);
        const zone = Zone2.current;
        if (this[symbolState] == UNRESOLVED) {
          this[symbolValue].push(zone, chainPromise, onFulfilled, onRejected);
        } else {
          scheduleResolveOrReject(this, zone, chainPromise, onFulfilled, onRejected);
        }
        return chainPromise;
      }
      catch(onRejected) {
        return this.then(null, onRejected);
      }
      finally(onFinally) {
        let C2 = this.constructor[Symbol.species];
        if (!C2 || typeof C2 !== "function") {
          C2 = ZoneAwarePromise;
        }
        const chainPromise = new C2(noop);
        chainPromise[symbolFinally] = symbolFinally;
        const zone = Zone2.current;
        if (this[symbolState] == UNRESOLVED) {
          this[symbolValue].push(zone, chainPromise, onFinally, onFinally);
        } else {
          scheduleResolveOrReject(this, zone, chainPromise, onFinally, onFinally);
        }
        return chainPromise;
      }
    }
    ZoneAwarePromise["resolve"] = ZoneAwarePromise.resolve;
    ZoneAwarePromise["reject"] = ZoneAwarePromise.reject;
    ZoneAwarePromise["race"] = ZoneAwarePromise.race;
    ZoneAwarePromise["all"] = ZoneAwarePromise.all;
    const NativePromise = global2[symbolPromise] = global2["Promise"];
    global2["Promise"] = ZoneAwarePromise;
    const symbolThenPatched = __symbol__("thenPatched");
    function patchThen(Ctor) {
      const proto = Ctor.prototype;
      const prop = ObjectGetOwnPropertyDescriptor2(proto, "then");
      if (prop && (prop.writable === false || !prop.configurable)) {
        return;
      }
      const originalThen = proto.then;
      proto[symbolThen] = originalThen;
      Ctor.prototype.then = function(onResolve, onReject) {
        const wrapped = new ZoneAwarePromise((resolve, reject) => {
          originalThen.call(this, resolve, reject);
        });
        return wrapped.then(onResolve, onReject);
      };
      Ctor[symbolThenPatched] = true;
    }
    api.patchThen = patchThen;
    function zoneify(fn) {
      return function(self2, args) {
        let resultPromise = fn.apply(self2, args);
        if (resultPromise instanceof ZoneAwarePromise) {
          return resultPromise;
        }
        let ctor = resultPromise.constructor;
        if (!ctor[symbolThenPatched]) {
          patchThen(ctor);
        }
        return resultPromise;
      };
    }
    if (NativePromise) {
      patchThen(NativePromise);
      patchMethod(global2, "fetch", (delegate) => zoneify(delegate));
    }
    Promise[Zone2.__symbol__("uncaughtPromiseErrors")] = _uncaughtPromiseErrors;
    return ZoneAwarePromise;
  });
  Zone.__load_patch("toString", (global2) => {
    const originalFunctionToString = Function.prototype.toString;
    const ORIGINAL_DELEGATE_SYMBOL = zoneSymbol("OriginalDelegate");
    const PROMISE_SYMBOL = zoneSymbol("Promise");
    const ERROR_SYMBOL = zoneSymbol("Error");
    const newFunctionToString = function toString() {
      if (typeof this === "function") {
        const originalDelegate = this[ORIGINAL_DELEGATE_SYMBOL];
        if (originalDelegate) {
          if (typeof originalDelegate === "function") {
            return originalFunctionToString.call(originalDelegate);
          } else {
            return Object.prototype.toString.call(originalDelegate);
          }
        }
        if (this === Promise) {
          const nativePromise = global2[PROMISE_SYMBOL];
          if (nativePromise) {
            return originalFunctionToString.call(nativePromise);
          }
        }
        if (this === Error) {
          const nativeError = global2[ERROR_SYMBOL];
          if (nativeError) {
            return originalFunctionToString.call(nativeError);
          }
        }
      }
      return originalFunctionToString.call(this);
    };
    newFunctionToString[ORIGINAL_DELEGATE_SYMBOL] = originalFunctionToString;
    Function.prototype.toString = newFunctionToString;
    const originalObjectToString = Object.prototype.toString;
    const PROMISE_OBJECT_TO_STRING = "[object Promise]";
    Object.prototype.toString = function() {
      if (typeof Promise === "function" && this instanceof Promise) {
        return PROMISE_OBJECT_TO_STRING;
      }
      return originalObjectToString.call(this);
    };
  });
  var passiveSupported = false;
  if (typeof window !== "undefined") {
    try {
      const options = Object.defineProperty({}, "passive", {
        get: function() {
          passiveSupported = true;
        }
      });
      window.addEventListener("test", options, options);
      window.removeEventListener("test", options, options);
    } catch (err) {
      passiveSupported = false;
    }
  }
  var OPTIMIZED_ZONE_EVENT_TASK_DATA = {
    useG: true
  };
  var zoneSymbolEventNames$1 = {};
  var globalSources = {};
  var EVENT_NAME_SYMBOL_REGX = new RegExp("^" + ZONE_SYMBOL_PREFIX + "(\\w+)(true|false)$");
  var IMMEDIATE_PROPAGATION_SYMBOL = zoneSymbol("propagationStopped");
  function prepareEventNames(eventName, eventNameToString) {
    const falseEventName = (eventNameToString ? eventNameToString(eventName) : eventName) + FALSE_STR;
    const trueEventName = (eventNameToString ? eventNameToString(eventName) : eventName) + TRUE_STR;
    const symbol = ZONE_SYMBOL_PREFIX + falseEventName;
    const symbolCapture = ZONE_SYMBOL_PREFIX + trueEventName;
    zoneSymbolEventNames$1[eventName] = {};
    zoneSymbolEventNames$1[eventName][FALSE_STR] = symbol;
    zoneSymbolEventNames$1[eventName][TRUE_STR] = symbolCapture;
  }
  function patchEventTarget(_global3, apis, patchOptions) {
    const ADD_EVENT_LISTENER = patchOptions && patchOptions.add || ADD_EVENT_LISTENER_STR;
    const REMOVE_EVENT_LISTENER = patchOptions && patchOptions.rm || REMOVE_EVENT_LISTENER_STR;
    const LISTENERS_EVENT_LISTENER = patchOptions && patchOptions.listeners || "eventListeners";
    const REMOVE_ALL_LISTENERS_EVENT_LISTENER = patchOptions && patchOptions.rmAll || "removeAllListeners";
    const zoneSymbolAddEventListener = zoneSymbol(ADD_EVENT_LISTENER);
    const ADD_EVENT_LISTENER_SOURCE = "." + ADD_EVENT_LISTENER + ":";
    const PREPEND_EVENT_LISTENER = "prependListener";
    const PREPEND_EVENT_LISTENER_SOURCE = "." + PREPEND_EVENT_LISTENER + ":";
    const invokeTask = function(task, target, event) {
      if (task.isRemoved) {
        return;
      }
      const delegate = task.callback;
      if (typeof delegate === "object" && delegate.handleEvent) {
        task.callback = (event2) => delegate.handleEvent(event2);
        task.originalDelegate = delegate;
      }
      task.invoke(task, target, [event]);
      const options = task.options;
      if (options && typeof options === "object" && options.once) {
        const delegate2 = task.originalDelegate ? task.originalDelegate : task.callback;
        target[REMOVE_EVENT_LISTENER].call(target, event.type, delegate2, options);
      }
    };
    const globalZoneAwareCallback = function(event) {
      event = event || _global3.event;
      if (!event) {
        return;
      }
      const target = this || event.target || _global3;
      const tasks = target[zoneSymbolEventNames$1[event.type][FALSE_STR]];
      if (tasks) {
        if (tasks.length === 1) {
          invokeTask(tasks[0], target, event);
        } else {
          const copyTasks = tasks.slice();
          for (let i2 = 0; i2 < copyTasks.length; i2++) {
            if (event && event[IMMEDIATE_PROPAGATION_SYMBOL] === true) {
              break;
            }
            invokeTask(copyTasks[i2], target, event);
          }
        }
      }
    };
    const globalZoneAwareCaptureCallback = function(event) {
      event = event || _global3.event;
      if (!event) {
        return;
      }
      const target = this || event.target || _global3;
      const tasks = target[zoneSymbolEventNames$1[event.type][TRUE_STR]];
      if (tasks) {
        if (tasks.length === 1) {
          invokeTask(tasks[0], target, event);
        } else {
          const copyTasks = tasks.slice();
          for (let i2 = 0; i2 < copyTasks.length; i2++) {
            if (event && event[IMMEDIATE_PROPAGATION_SYMBOL] === true) {
              break;
            }
            invokeTask(copyTasks[i2], target, event);
          }
        }
      }
    };
    function patchEventTargetMethods(obj, patchOptions2) {
      if (!obj) {
        return false;
      }
      let useGlobalCallback = true;
      if (patchOptions2 && patchOptions2.useG !== void 0) {
        useGlobalCallback = patchOptions2.useG;
      }
      const validateHandler = patchOptions2 && patchOptions2.vh;
      let checkDuplicate = true;
      if (patchOptions2 && patchOptions2.chkDup !== void 0) {
        checkDuplicate = patchOptions2.chkDup;
      }
      let returnTarget = false;
      if (patchOptions2 && patchOptions2.rt !== void 0) {
        returnTarget = patchOptions2.rt;
      }
      let proto = obj;
      while (proto && !proto.hasOwnProperty(ADD_EVENT_LISTENER)) {
        proto = ObjectGetPrototypeOf(proto);
      }
      if (!proto && obj[ADD_EVENT_LISTENER]) {
        proto = obj;
      }
      if (!proto) {
        return false;
      }
      if (proto[zoneSymbolAddEventListener]) {
        return false;
      }
      const eventNameToString = patchOptions2 && patchOptions2.eventNameToString;
      const taskData = {};
      const nativeAddEventListener = proto[zoneSymbolAddEventListener] = proto[ADD_EVENT_LISTENER];
      const nativeRemoveEventListener = proto[zoneSymbol(REMOVE_EVENT_LISTENER)] = proto[REMOVE_EVENT_LISTENER];
      const nativeListeners = proto[zoneSymbol(LISTENERS_EVENT_LISTENER)] = proto[LISTENERS_EVENT_LISTENER];
      const nativeRemoveAllListeners = proto[zoneSymbol(REMOVE_ALL_LISTENERS_EVENT_LISTENER)] = proto[REMOVE_ALL_LISTENERS_EVENT_LISTENER];
      let nativePrependEventListener;
      if (patchOptions2 && patchOptions2.prepend) {
        nativePrependEventListener = proto[zoneSymbol(patchOptions2.prepend)] = proto[patchOptions2.prepend];
      }
      function buildEventListenerOptions(options, passive) {
        if (!passiveSupported && typeof options === "object" && options) {
          return !!options.capture;
        }
        if (!passiveSupported || !passive) {
          return options;
        }
        if (typeof options === "boolean") {
          return { capture: options, passive: true };
        }
        if (!options) {
          return { passive: true };
        }
        if (typeof options === "object" && options.passive !== false) {
          return Object.assign(Object.assign({}, options), { passive: true });
        }
        return options;
      }
      const customScheduleGlobal = function(task) {
        if (taskData.isExisting) {
          return;
        }
        return nativeAddEventListener.call(taskData.target, taskData.eventName, taskData.capture ? globalZoneAwareCaptureCallback : globalZoneAwareCallback, taskData.options);
      };
      const customCancelGlobal = function(task) {
        if (!task.isRemoved) {
          const symbolEventNames = zoneSymbolEventNames$1[task.eventName];
          let symbolEventName;
          if (symbolEventNames) {
            symbolEventName = symbolEventNames[task.capture ? TRUE_STR : FALSE_STR];
          }
          const existingTasks = symbolEventName && task.target[symbolEventName];
          if (existingTasks) {
            for (let i2 = 0; i2 < existingTasks.length; i2++) {
              const existingTask = existingTasks[i2];
              if (existingTask === task) {
                existingTasks.splice(i2, 1);
                task.isRemoved = true;
                if (existingTasks.length === 0) {
                  task.allRemoved = true;
                  task.target[symbolEventName] = null;
                }
                break;
              }
            }
          }
        }
        if (!task.allRemoved) {
          return;
        }
        return nativeRemoveEventListener.call(task.target, task.eventName, task.capture ? globalZoneAwareCaptureCallback : globalZoneAwareCallback, task.options);
      };
      const customScheduleNonGlobal = function(task) {
        return nativeAddEventListener.call(taskData.target, taskData.eventName, task.invoke, taskData.options);
      };
      const customSchedulePrepend = function(task) {
        return nativePrependEventListener.call(taskData.target, taskData.eventName, task.invoke, taskData.options);
      };
      const customCancelNonGlobal = function(task) {
        return nativeRemoveEventListener.call(task.target, task.eventName, task.invoke, task.options);
      };
      const customSchedule = useGlobalCallback ? customScheduleGlobal : customScheduleNonGlobal;
      const customCancel = useGlobalCallback ? customCancelGlobal : customCancelNonGlobal;
      const compareTaskCallbackVsDelegate = function(task, delegate) {
        const typeOfDelegate = typeof delegate;
        return typeOfDelegate === "function" && task.callback === delegate || typeOfDelegate === "object" && task.originalDelegate === delegate;
      };
      const compare = patchOptions2 && patchOptions2.diff ? patchOptions2.diff : compareTaskCallbackVsDelegate;
      const unpatchedEvents = Zone[zoneSymbol("UNPATCHED_EVENTS")];
      const passiveEvents = _global3[zoneSymbol("PASSIVE_EVENTS")];
      const makeAddListener = function(nativeListener, addSource, customScheduleFn, customCancelFn, returnTarget2 = false, prepend = false) {
        return function() {
          const target = this || _global3;
          let eventName = arguments[0];
          if (patchOptions2 && patchOptions2.transferEventName) {
            eventName = patchOptions2.transferEventName(eventName);
          }
          let delegate = arguments[1];
          if (!delegate) {
            return nativeListener.apply(this, arguments);
          }
          if (isNode && eventName === "uncaughtException") {
            return nativeListener.apply(this, arguments);
          }
          let isHandleEvent = false;
          if (typeof delegate !== "function") {
            if (!delegate.handleEvent) {
              return nativeListener.apply(this, arguments);
            }
            isHandleEvent = true;
          }
          if (validateHandler && !validateHandler(nativeListener, delegate, target, arguments)) {
            return;
          }
          const passive = passiveSupported && !!passiveEvents && passiveEvents.indexOf(eventName) !== -1;
          const options = buildEventListenerOptions(arguments[2], passive);
          if (unpatchedEvents) {
            for (let i2 = 0; i2 < unpatchedEvents.length; i2++) {
              if (eventName === unpatchedEvents[i2]) {
                if (passive) {
                  return nativeListener.call(target, eventName, delegate, options);
                } else {
                  return nativeListener.apply(this, arguments);
                }
              }
            }
          }
          const capture = !options ? false : typeof options === "boolean" ? true : options.capture;
          const once = options && typeof options === "object" ? options.once : false;
          const zone = Zone.current;
          let symbolEventNames = zoneSymbolEventNames$1[eventName];
          if (!symbolEventNames) {
            prepareEventNames(eventName, eventNameToString);
            symbolEventNames = zoneSymbolEventNames$1[eventName];
          }
          const symbolEventName = symbolEventNames[capture ? TRUE_STR : FALSE_STR];
          let existingTasks = target[symbolEventName];
          let isExisting = false;
          if (existingTasks) {
            isExisting = true;
            if (checkDuplicate) {
              for (let i2 = 0; i2 < existingTasks.length; i2++) {
                if (compare(existingTasks[i2], delegate)) {
                  return;
                }
              }
            }
          } else {
            existingTasks = target[symbolEventName] = [];
          }
          let source;
          const constructorName = target.constructor["name"];
          const targetSource = globalSources[constructorName];
          if (targetSource) {
            source = targetSource[eventName];
          }
          if (!source) {
            source = constructorName + addSource + (eventNameToString ? eventNameToString(eventName) : eventName);
          }
          taskData.options = options;
          if (once) {
            taskData.options.once = false;
          }
          taskData.target = target;
          taskData.capture = capture;
          taskData.eventName = eventName;
          taskData.isExisting = isExisting;
          const data = useGlobalCallback ? OPTIMIZED_ZONE_EVENT_TASK_DATA : void 0;
          if (data) {
            data.taskData = taskData;
          }
          const task = zone.scheduleEventTask(source, delegate, data, customScheduleFn, customCancelFn);
          taskData.target = null;
          if (data) {
            data.taskData = null;
          }
          if (once) {
            options.once = true;
          }
          if (!(!passiveSupported && typeof task.options === "boolean")) {
            task.options = options;
          }
          task.target = target;
          task.capture = capture;
          task.eventName = eventName;
          if (isHandleEvent) {
            task.originalDelegate = delegate;
          }
          if (!prepend) {
            existingTasks.push(task);
          } else {
            existingTasks.unshift(task);
          }
          if (returnTarget2) {
            return target;
          }
        };
      };
      proto[ADD_EVENT_LISTENER] = makeAddListener(nativeAddEventListener, ADD_EVENT_LISTENER_SOURCE, customSchedule, customCancel, returnTarget);
      if (nativePrependEventListener) {
        proto[PREPEND_EVENT_LISTENER] = makeAddListener(nativePrependEventListener, PREPEND_EVENT_LISTENER_SOURCE, customSchedulePrepend, customCancel, returnTarget, true);
      }
      proto[REMOVE_EVENT_LISTENER] = function() {
        const target = this || _global3;
        let eventName = arguments[0];
        if (patchOptions2 && patchOptions2.transferEventName) {
          eventName = patchOptions2.transferEventName(eventName);
        }
        const options = arguments[2];
        const capture = !options ? false : typeof options === "boolean" ? true : options.capture;
        const delegate = arguments[1];
        if (!delegate) {
          return nativeRemoveEventListener.apply(this, arguments);
        }
        if (validateHandler && !validateHandler(nativeRemoveEventListener, delegate, target, arguments)) {
          return;
        }
        const symbolEventNames = zoneSymbolEventNames$1[eventName];
        let symbolEventName;
        if (symbolEventNames) {
          symbolEventName = symbolEventNames[capture ? TRUE_STR : FALSE_STR];
        }
        const existingTasks = symbolEventName && target[symbolEventName];
        if (existingTasks) {
          for (let i2 = 0; i2 < existingTasks.length; i2++) {
            const existingTask = existingTasks[i2];
            if (compare(existingTask, delegate)) {
              existingTasks.splice(i2, 1);
              existingTask.isRemoved = true;
              if (existingTasks.length === 0) {
                existingTask.allRemoved = true;
                target[symbolEventName] = null;
                if (typeof eventName === "string") {
                  const onPropertySymbol = ZONE_SYMBOL_PREFIX + "ON_PROPERTY" + eventName;
                  target[onPropertySymbol] = null;
                }
              }
              existingTask.zone.cancelTask(existingTask);
              if (returnTarget) {
                return target;
              }
              return;
            }
          }
        }
        return nativeRemoveEventListener.apply(this, arguments);
      };
      proto[LISTENERS_EVENT_LISTENER] = function() {
        const target = this || _global3;
        let eventName = arguments[0];
        if (patchOptions2 && patchOptions2.transferEventName) {
          eventName = patchOptions2.transferEventName(eventName);
        }
        const listeners = [];
        const tasks = findEventTasks(target, eventNameToString ? eventNameToString(eventName) : eventName);
        for (let i2 = 0; i2 < tasks.length; i2++) {
          const task = tasks[i2];
          let delegate = task.originalDelegate ? task.originalDelegate : task.callback;
          listeners.push(delegate);
        }
        return listeners;
      };
      proto[REMOVE_ALL_LISTENERS_EVENT_LISTENER] = function() {
        const target = this || _global3;
        let eventName = arguments[0];
        if (!eventName) {
          const keys = Object.keys(target);
          for (let i2 = 0; i2 < keys.length; i2++) {
            const prop = keys[i2];
            const match = EVENT_NAME_SYMBOL_REGX.exec(prop);
            let evtName = match && match[1];
            if (evtName && evtName !== "removeListener") {
              this[REMOVE_ALL_LISTENERS_EVENT_LISTENER].call(this, evtName);
            }
          }
          this[REMOVE_ALL_LISTENERS_EVENT_LISTENER].call(this, "removeListener");
        } else {
          if (patchOptions2 && patchOptions2.transferEventName) {
            eventName = patchOptions2.transferEventName(eventName);
          }
          const symbolEventNames = zoneSymbolEventNames$1[eventName];
          if (symbolEventNames) {
            const symbolEventName = symbolEventNames[FALSE_STR];
            const symbolCaptureEventName = symbolEventNames[TRUE_STR];
            const tasks = target[symbolEventName];
            const captureTasks = target[symbolCaptureEventName];
            if (tasks) {
              const removeTasks = tasks.slice();
              for (let i2 = 0; i2 < removeTasks.length; i2++) {
                const task = removeTasks[i2];
                let delegate = task.originalDelegate ? task.originalDelegate : task.callback;
                this[REMOVE_EVENT_LISTENER].call(this, eventName, delegate, task.options);
              }
            }
            if (captureTasks) {
              const removeTasks = captureTasks.slice();
              for (let i2 = 0; i2 < removeTasks.length; i2++) {
                const task = removeTasks[i2];
                let delegate = task.originalDelegate ? task.originalDelegate : task.callback;
                this[REMOVE_EVENT_LISTENER].call(this, eventName, delegate, task.options);
              }
            }
          }
        }
        if (returnTarget) {
          return this;
        }
      };
      attachOriginToPatched(proto[ADD_EVENT_LISTENER], nativeAddEventListener);
      attachOriginToPatched(proto[REMOVE_EVENT_LISTENER], nativeRemoveEventListener);
      if (nativeRemoveAllListeners) {
        attachOriginToPatched(proto[REMOVE_ALL_LISTENERS_EVENT_LISTENER], nativeRemoveAllListeners);
      }
      if (nativeListeners) {
        attachOriginToPatched(proto[LISTENERS_EVENT_LISTENER], nativeListeners);
      }
      return true;
    }
    let results = [];
    for (let i2 = 0; i2 < apis.length; i2++) {
      results[i2] = patchEventTargetMethods(apis[i2], patchOptions);
    }
    return results;
  }
  function findEventTasks(target, eventName) {
    if (!eventName) {
      const foundTasks = [];
      for (let prop in target) {
        const match = EVENT_NAME_SYMBOL_REGX.exec(prop);
        let evtName = match && match[1];
        if (evtName && (!eventName || evtName === eventName)) {
          const tasks = target[prop];
          if (tasks) {
            for (let i2 = 0; i2 < tasks.length; i2++) {
              foundTasks.push(tasks[i2]);
            }
          }
        }
      }
      return foundTasks;
    }
    let symbolEventName = zoneSymbolEventNames$1[eventName];
    if (!symbolEventName) {
      prepareEventNames(eventName);
      symbolEventName = zoneSymbolEventNames$1[eventName];
    }
    const captureFalseTasks = target[symbolEventName[FALSE_STR]];
    const captureTrueTasks = target[symbolEventName[TRUE_STR]];
    if (!captureFalseTasks) {
      return captureTrueTasks ? captureTrueTasks.slice() : [];
    } else {
      return captureTrueTasks ? captureFalseTasks.concat(captureTrueTasks) : captureFalseTasks.slice();
    }
  }
  function patchEventPrototype(global2, api) {
    const Event = global2["Event"];
    if (Event && Event.prototype) {
      api.patchMethod(Event.prototype, "stopImmediatePropagation", (delegate) => function(self2, args) {
        self2[IMMEDIATE_PROPAGATION_SYMBOL] = true;
        delegate && delegate.apply(self2, args);
      });
    }
  }
  function patchCallbacks(api, target, targetName, method, callbacks) {
    const symbol = Zone.__symbol__(method);
    if (target[symbol]) {
      return;
    }
    const nativeDelegate = target[symbol] = target[method];
    target[method] = function(name, opts, options) {
      if (opts && opts.prototype) {
        callbacks.forEach(function(callback) {
          const source = `${targetName}.${method}::` + callback;
          const prototype = opts.prototype;
          if (prototype.hasOwnProperty(callback)) {
            const descriptor = api.ObjectGetOwnPropertyDescriptor(prototype, callback);
            if (descriptor && descriptor.value) {
              descriptor.value = api.wrapWithCurrentZone(descriptor.value, source);
              api._redefineProperty(opts.prototype, callback, descriptor);
            } else if (prototype[callback]) {
              prototype[callback] = api.wrapWithCurrentZone(prototype[callback], source);
            }
          } else if (prototype[callback]) {
            prototype[callback] = api.wrapWithCurrentZone(prototype[callback], source);
          }
        });
      }
      return nativeDelegate.call(target, name, opts, options);
    };
    api.attachOriginToPatched(target[method], nativeDelegate);
  }
  var globalEventHandlersEventNames = [
    "abort",
    "animationcancel",
    "animationend",
    "animationiteration",
    "auxclick",
    "beforeinput",
    "blur",
    "cancel",
    "canplay",
    "canplaythrough",
    "change",
    "compositionstart",
    "compositionupdate",
    "compositionend",
    "cuechange",
    "click",
    "close",
    "contextmenu",
    "curechange",
    "dblclick",
    "drag",
    "dragend",
    "dragenter",
    "dragexit",
    "dragleave",
    "dragover",
    "drop",
    "durationchange",
    "emptied",
    "ended",
    "error",
    "focus",
    "focusin",
    "focusout",
    "gotpointercapture",
    "input",
    "invalid",
    "keydown",
    "keypress",
    "keyup",
    "load",
    "loadstart",
    "loadeddata",
    "loadedmetadata",
    "lostpointercapture",
    "mousedown",
    "mouseenter",
    "mouseleave",
    "mousemove",
    "mouseout",
    "mouseover",
    "mouseup",
    "mousewheel",
    "orientationchange",
    "pause",
    "play",
    "playing",
    "pointercancel",
    "pointerdown",
    "pointerenter",
    "pointerleave",
    "pointerlockchange",
    "mozpointerlockchange",
    "webkitpointerlockerchange",
    "pointerlockerror",
    "mozpointerlockerror",
    "webkitpointerlockerror",
    "pointermove",
    "pointout",
    "pointerover",
    "pointerup",
    "progress",
    "ratechange",
    "reset",
    "resize",
    "scroll",
    "seeked",
    "seeking",
    "select",
    "selectionchange",
    "selectstart",
    "show",
    "sort",
    "stalled",
    "submit",
    "suspend",
    "timeupdate",
    "volumechange",
    "touchcancel",
    "touchmove",
    "touchstart",
    "touchend",
    "transitioncancel",
    "transitionend",
    "waiting",
    "wheel"
  ];
  var documentEventNames = [
    "afterscriptexecute",
    "beforescriptexecute",
    "DOMContentLoaded",
    "freeze",
    "fullscreenchange",
    "mozfullscreenchange",
    "webkitfullscreenchange",
    "msfullscreenchange",
    "fullscreenerror",
    "mozfullscreenerror",
    "webkitfullscreenerror",
    "msfullscreenerror",
    "readystatechange",
    "visibilitychange",
    "resume"
  ];
  var windowEventNames = [
    "absolutedeviceorientation",
    "afterinput",
    "afterprint",
    "appinstalled",
    "beforeinstallprompt",
    "beforeprint",
    "beforeunload",
    "devicelight",
    "devicemotion",
    "deviceorientation",
    "deviceorientationabsolute",
    "deviceproximity",
    "hashchange",
    "languagechange",
    "message",
    "mozbeforepaint",
    "offline",
    "online",
    "paint",
    "pageshow",
    "pagehide",
    "popstate",
    "rejectionhandled",
    "storage",
    "unhandledrejection",
    "unload",
    "userproximity",
    "vrdisplayconnected",
    "vrdisplaydisconnected",
    "vrdisplaypresentchange"
  ];
  var htmlElementEventNames = [
    "beforecopy",
    "beforecut",
    "beforepaste",
    "copy",
    "cut",
    "paste",
    "dragstart",
    "loadend",
    "animationstart",
    "search",
    "transitionrun",
    "transitionstart",
    "webkitanimationend",
    "webkitanimationiteration",
    "webkitanimationstart",
    "webkittransitionend"
  ];
  var mediaElementEventNames = ["encrypted", "waitingforkey", "msneedkey", "mozinterruptbegin", "mozinterruptend"];
  var ieElementEventNames = [
    "activate",
    "afterupdate",
    "ariarequest",
    "beforeactivate",
    "beforedeactivate",
    "beforeeditfocus",
    "beforeupdate",
    "cellchange",
    "controlselect",
    "dataavailable",
    "datasetchanged",
    "datasetcomplete",
    "errorupdate",
    "filterchange",
    "layoutcomplete",
    "losecapture",
    "move",
    "moveend",
    "movestart",
    "propertychange",
    "resizeend",
    "resizestart",
    "rowenter",
    "rowexit",
    "rowsdelete",
    "rowsinserted",
    "command",
    "compassneedscalibration",
    "deactivate",
    "help",
    "mscontentzoom",
    "msmanipulationstatechanged",
    "msgesturechange",
    "msgesturedoubletap",
    "msgestureend",
    "msgesturehold",
    "msgesturestart",
    "msgesturetap",
    "msgotpointercapture",
    "msinertiastart",
    "mslostpointercapture",
    "mspointercancel",
    "mspointerdown",
    "mspointerenter",
    "mspointerhover",
    "mspointerleave",
    "mspointermove",
    "mspointerout",
    "mspointerover",
    "mspointerup",
    "pointerout",
    "mssitemodejumplistitemremoved",
    "msthumbnailclick",
    "stop",
    "storagecommit"
  ];
  var webglEventNames = ["webglcontextrestored", "webglcontextlost", "webglcontextcreationerror"];
  var formEventNames = ["autocomplete", "autocompleteerror"];
  var detailEventNames = ["toggle"];
  var frameEventNames = ["load"];
  var frameSetEventNames = ["blur", "error", "focus", "load", "resize", "scroll", "messageerror"];
  var marqueeEventNames = ["bounce", "finish", "start"];
  var XMLHttpRequestEventNames = [
    "loadstart",
    "progress",
    "abort",
    "error",
    "load",
    "progress",
    "timeout",
    "loadend",
    "readystatechange"
  ];
  var IDBIndexEventNames = ["upgradeneeded", "complete", "abort", "success", "error", "blocked", "versionchange", "close"];
  var websocketEventNames = ["close", "error", "open", "message"];
  var workerEventNames = ["error", "message"];
  var eventNames = globalEventHandlersEventNames.concat(webglEventNames, formEventNames, detailEventNames, documentEventNames, windowEventNames, htmlElementEventNames, ieElementEventNames);
  function filterProperties(target, onProperties, ignoreProperties) {
    if (!ignoreProperties || ignoreProperties.length === 0) {
      return onProperties;
    }
    const tip = ignoreProperties.filter((ip) => ip.target === target);
    if (!tip || tip.length === 0) {
      return onProperties;
    }
    const targetIgnoreProperties = tip[0].ignoreProperties;
    return onProperties.filter((op) => targetIgnoreProperties.indexOf(op) === -1);
  }
  function patchFilteredProperties(target, onProperties, ignoreProperties, prototype) {
    if (!target) {
      return;
    }
    const filteredProperties = filterProperties(target, onProperties, ignoreProperties);
    patchOnProperties(target, filteredProperties, prototype);
  }
  function propertyDescriptorPatch(api, _global3) {
    if (isNode && !isMix) {
      return;
    }
    if (Zone[api.symbol("patchEvents")]) {
      return;
    }
    const supportsWebSocket = typeof WebSocket !== "undefined";
    const ignoreProperties = _global3["__Zone_ignore_on_properties"];
    if (isBrowser) {
      const internalWindow2 = window;
      const ignoreErrorProperties = isIE() ? [{ target: internalWindow2, ignoreProperties: ["error"] }] : [];
      patchFilteredProperties(internalWindow2, eventNames.concat(["messageerror"]), ignoreProperties ? ignoreProperties.concat(ignoreErrorProperties) : ignoreProperties, ObjectGetPrototypeOf(internalWindow2));
      patchFilteredProperties(Document.prototype, eventNames, ignoreProperties);
      if (typeof internalWindow2["SVGElement"] !== "undefined") {
        patchFilteredProperties(internalWindow2["SVGElement"].prototype, eventNames, ignoreProperties);
      }
      patchFilteredProperties(Element.prototype, eventNames, ignoreProperties);
      patchFilteredProperties(HTMLElement.prototype, eventNames, ignoreProperties);
      patchFilteredProperties(HTMLMediaElement.prototype, mediaElementEventNames, ignoreProperties);
      patchFilteredProperties(HTMLFrameSetElement.prototype, windowEventNames.concat(frameSetEventNames), ignoreProperties);
      patchFilteredProperties(HTMLBodyElement.prototype, windowEventNames.concat(frameSetEventNames), ignoreProperties);
      patchFilteredProperties(HTMLFrameElement.prototype, frameEventNames, ignoreProperties);
      patchFilteredProperties(HTMLIFrameElement.prototype, frameEventNames, ignoreProperties);
      const HTMLMarqueeElement = internalWindow2["HTMLMarqueeElement"];
      if (HTMLMarqueeElement) {
        patchFilteredProperties(HTMLMarqueeElement.prototype, marqueeEventNames, ignoreProperties);
      }
      const Worker = internalWindow2["Worker"];
      if (Worker) {
        patchFilteredProperties(Worker.prototype, workerEventNames, ignoreProperties);
      }
    }
    const XMLHttpRequest2 = _global3["XMLHttpRequest"];
    if (XMLHttpRequest2) {
      patchFilteredProperties(XMLHttpRequest2.prototype, XMLHttpRequestEventNames, ignoreProperties);
    }
    const XMLHttpRequestEventTarget = _global3["XMLHttpRequestEventTarget"];
    if (XMLHttpRequestEventTarget) {
      patchFilteredProperties(XMLHttpRequestEventTarget && XMLHttpRequestEventTarget.prototype, XMLHttpRequestEventNames, ignoreProperties);
    }
    if (typeof IDBIndex !== "undefined") {
      patchFilteredProperties(IDBIndex.prototype, IDBIndexEventNames, ignoreProperties);
      patchFilteredProperties(IDBRequest.prototype, IDBIndexEventNames, ignoreProperties);
      patchFilteredProperties(IDBOpenDBRequest.prototype, IDBIndexEventNames, ignoreProperties);
      patchFilteredProperties(IDBDatabase.prototype, IDBIndexEventNames, ignoreProperties);
      patchFilteredProperties(IDBTransaction.prototype, IDBIndexEventNames, ignoreProperties);
      patchFilteredProperties(IDBCursor.prototype, IDBIndexEventNames, ignoreProperties);
    }
    if (supportsWebSocket) {
      patchFilteredProperties(WebSocket.prototype, websocketEventNames, ignoreProperties);
    }
  }
  Zone.__load_patch("util", (global2, Zone2, api) => {
    api.patchOnProperties = patchOnProperties;
    api.patchMethod = patchMethod;
    api.bindArguments = bindArguments;
    api.patchMacroTask = patchMacroTask;
    const SYMBOL_BLACK_LISTED_EVENTS = Zone2.__symbol__("BLACK_LISTED_EVENTS");
    const SYMBOL_UNPATCHED_EVENTS = Zone2.__symbol__("UNPATCHED_EVENTS");
    if (global2[SYMBOL_UNPATCHED_EVENTS]) {
      global2[SYMBOL_BLACK_LISTED_EVENTS] = global2[SYMBOL_UNPATCHED_EVENTS];
    }
    if (global2[SYMBOL_BLACK_LISTED_EVENTS]) {
      Zone2[SYMBOL_BLACK_LISTED_EVENTS] = Zone2[SYMBOL_UNPATCHED_EVENTS] = global2[SYMBOL_BLACK_LISTED_EVENTS];
    }
    api.patchEventPrototype = patchEventPrototype;
    api.patchEventTarget = patchEventTarget;
    api.isIEOrEdge = isIEOrEdge;
    api.ObjectDefineProperty = ObjectDefineProperty;
    api.ObjectGetOwnPropertyDescriptor = ObjectGetOwnPropertyDescriptor;
    api.ObjectCreate = ObjectCreate;
    api.ArraySlice = ArraySlice;
    api.patchClass = patchClass;
    api.wrapWithCurrentZone = wrapWithCurrentZone;
    api.filterProperties = filterProperties;
    api.attachOriginToPatched = attachOriginToPatched;
    api._redefineProperty = Object.defineProperty;
    api.patchCallbacks = patchCallbacks;
    api.getGlobalObjects = () => ({
      globalSources,
      zoneSymbolEventNames: zoneSymbolEventNames$1,
      eventNames,
      isBrowser,
      isMix,
      isNode,
      TRUE_STR,
      FALSE_STR,
      ZONE_SYMBOL_PREFIX,
      ADD_EVENT_LISTENER_STR,
      REMOVE_EVENT_LISTENER_STR
    });
  });
  var taskSymbol = zoneSymbol("zoneTask");
  function patchTimer(window2, setName, cancelName, nameSuffix) {
    let setNative = null;
    let clearNative = null;
    setName += nameSuffix;
    cancelName += nameSuffix;
    const tasksByHandleId = {};
    function scheduleTask(task) {
      const data = task.data;
      data.args[0] = function() {
        return task.invoke.apply(this, arguments);
      };
      data.handleId = setNative.apply(window2, data.args);
      return task;
    }
    function clearTask(task) {
      return clearNative.call(window2, task.data.handleId);
    }
    setNative = patchMethod(window2, setName, (delegate) => function(self2, args) {
      if (typeof args[0] === "function") {
        const options = {
          isPeriodic: nameSuffix === "Interval",
          delay: nameSuffix === "Timeout" || nameSuffix === "Interval" ? args[1] || 0 : void 0,
          args
        };
        const callback = args[0];
        args[0] = function timer() {
          try {
            return callback.apply(this, arguments);
          } finally {
            if (!options.isPeriodic) {
              if (typeof options.handleId === "number") {
                delete tasksByHandleId[options.handleId];
              } else if (options.handleId) {
                options.handleId[taskSymbol] = null;
              }
            }
          }
        };
        const task = scheduleMacroTaskWithCurrentZone(setName, args[0], options, scheduleTask, clearTask);
        if (!task) {
          return task;
        }
        const handle = task.data.handleId;
        if (typeof handle === "number") {
          tasksByHandleId[handle] = task;
        } else if (handle) {
          handle[taskSymbol] = task;
        }
        if (handle && handle.ref && handle.unref && typeof handle.ref === "function" && typeof handle.unref === "function") {
          task.ref = handle.ref.bind(handle);
          task.unref = handle.unref.bind(handle);
        }
        if (typeof handle === "number" || handle) {
          return handle;
        }
        return task;
      } else {
        return delegate.apply(window2, args);
      }
    });
    clearNative = patchMethod(window2, cancelName, (delegate) => function(self2, args) {
      const id = args[0];
      let task;
      if (typeof id === "number") {
        task = tasksByHandleId[id];
      } else {
        task = id && id[taskSymbol];
        if (!task) {
          task = id;
        }
      }
      if (task && typeof task.type === "string") {
        if (task.state !== "notScheduled" && (task.cancelFn && task.data.isPeriodic || task.runCount === 0)) {
          if (typeof id === "number") {
            delete tasksByHandleId[id];
          } else if (id) {
            id[taskSymbol] = null;
          }
          task.zone.cancelTask(task);
        }
      } else {
        delegate.apply(window2, args);
      }
    });
  }
  function patchCustomElements(_global3, api) {
    const { isBrowser: isBrowser2, isMix: isMix2 } = api.getGlobalObjects();
    if (!isBrowser2 && !isMix2 || !_global3["customElements"] || !("customElements" in _global3)) {
      return;
    }
    const callbacks = ["connectedCallback", "disconnectedCallback", "adoptedCallback", "attributeChangedCallback"];
    api.patchCallbacks(api, _global3.customElements, "customElements", "define", callbacks);
  }
  function eventTargetPatch(_global3, api) {
    if (Zone[api.symbol("patchEventTarget")]) {
      return;
    }
    const { eventNames: eventNames2, zoneSymbolEventNames: zoneSymbolEventNames2, TRUE_STR: TRUE_STR2, FALSE_STR: FALSE_STR2, ZONE_SYMBOL_PREFIX: ZONE_SYMBOL_PREFIX2 } = api.getGlobalObjects();
    for (let i2 = 0; i2 < eventNames2.length; i2++) {
      const eventName = eventNames2[i2];
      const falseEventName = eventName + FALSE_STR2;
      const trueEventName = eventName + TRUE_STR2;
      const symbol = ZONE_SYMBOL_PREFIX2 + falseEventName;
      const symbolCapture = ZONE_SYMBOL_PREFIX2 + trueEventName;
      zoneSymbolEventNames2[eventName] = {};
      zoneSymbolEventNames2[eventName][FALSE_STR2] = symbol;
      zoneSymbolEventNames2[eventName][TRUE_STR2] = symbolCapture;
    }
    const EVENT_TARGET = _global3["EventTarget"];
    if (!EVENT_TARGET || !EVENT_TARGET.prototype) {
      return;
    }
    api.patchEventTarget(_global3, [EVENT_TARGET && EVENT_TARGET.prototype]);
    return true;
  }
  function patchEvent(global2, api) {
    api.patchEventPrototype(global2, api);
  }
  Zone.__load_patch("legacy", (global2) => {
    const legacyPatch = global2[Zone.__symbol__("legacyPatch")];
    if (legacyPatch) {
      legacyPatch();
    }
  });
  Zone.__load_patch("queueMicrotask", (global2, Zone2, api) => {
    api.patchMethod(global2, "queueMicrotask", (delegate) => {
      return function(self2, args) {
        Zone2.current.scheduleMicroTask("queueMicrotask", args[0]);
      };
    });
  });
  Zone.__load_patch("timers", (global2) => {
    const set = "set";
    const clear = "clear";
    patchTimer(global2, set, clear, "Timeout");
    patchTimer(global2, set, clear, "Interval");
    patchTimer(global2, set, clear, "Immediate");
  });
  Zone.__load_patch("requestAnimationFrame", (global2) => {
    patchTimer(global2, "request", "cancel", "AnimationFrame");
    patchTimer(global2, "mozRequest", "mozCancel", "AnimationFrame");
    patchTimer(global2, "webkitRequest", "webkitCancel", "AnimationFrame");
  });
  Zone.__load_patch("blocking", (global2, Zone2) => {
    const blockingMethods = ["alert", "prompt", "confirm"];
    for (let i2 = 0; i2 < blockingMethods.length; i2++) {
      const name = blockingMethods[i2];
      patchMethod(global2, name, (delegate, symbol, name2) => {
        return function(s2, args) {
          return Zone2.current.run(delegate, global2, args, name2);
        };
      });
    }
  });
  Zone.__load_patch("EventTarget", (global2, Zone2, api) => {
    patchEvent(global2, api);
    eventTargetPatch(global2, api);
    const XMLHttpRequestEventTarget = global2["XMLHttpRequestEventTarget"];
    if (XMLHttpRequestEventTarget && XMLHttpRequestEventTarget.prototype) {
      api.patchEventTarget(global2, [XMLHttpRequestEventTarget.prototype]);
    }
  });
  Zone.__load_patch("MutationObserver", (global2, Zone2, api) => {
    patchClass("MutationObserver");
    patchClass("WebKitMutationObserver");
  });
  Zone.__load_patch("IntersectionObserver", (global2, Zone2, api) => {
    patchClass("IntersectionObserver");
  });
  Zone.__load_patch("FileReader", (global2, Zone2, api) => {
    patchClass("FileReader");
  });
  Zone.__load_patch("on_property", (global2, Zone2, api) => {
    propertyDescriptorPatch(api, global2);
  });
  Zone.__load_patch("customElements", (global2, Zone2, api) => {
    patchCustomElements(global2, api);
  });
  Zone.__load_patch("XHR", (global2, Zone2) => {
    patchXHR(global2);
    const XHR_TASK = zoneSymbol("xhrTask");
    const XHR_SYNC = zoneSymbol("xhrSync");
    const XHR_LISTENER = zoneSymbol("xhrListener");
    const XHR_SCHEDULED = zoneSymbol("xhrScheduled");
    const XHR_URL = zoneSymbol("xhrURL");
    const XHR_ERROR_BEFORE_SCHEDULED = zoneSymbol("xhrErrorBeforeScheduled");
    function patchXHR(window2) {
      const XMLHttpRequest2 = window2["XMLHttpRequest"];
      if (!XMLHttpRequest2) {
        return;
      }
      const XMLHttpRequestPrototype = XMLHttpRequest2.prototype;
      function findPendingTask(target) {
        return target[XHR_TASK];
      }
      let oriAddListener = XMLHttpRequestPrototype[ZONE_SYMBOL_ADD_EVENT_LISTENER];
      let oriRemoveListener = XMLHttpRequestPrototype[ZONE_SYMBOL_REMOVE_EVENT_LISTENER];
      if (!oriAddListener) {
        const XMLHttpRequestEventTarget = window2["XMLHttpRequestEventTarget"];
        if (XMLHttpRequestEventTarget) {
          const XMLHttpRequestEventTargetPrototype = XMLHttpRequestEventTarget.prototype;
          oriAddListener = XMLHttpRequestEventTargetPrototype[ZONE_SYMBOL_ADD_EVENT_LISTENER];
          oriRemoveListener = XMLHttpRequestEventTargetPrototype[ZONE_SYMBOL_REMOVE_EVENT_LISTENER];
        }
      }
      const READY_STATE_CHANGE = "readystatechange";
      const SCHEDULED = "scheduled";
      function scheduleTask(task) {
        const data = task.data;
        const target = data.target;
        target[XHR_SCHEDULED] = false;
        target[XHR_ERROR_BEFORE_SCHEDULED] = false;
        const listener = target[XHR_LISTENER];
        if (!oriAddListener) {
          oriAddListener = target[ZONE_SYMBOL_ADD_EVENT_LISTENER];
          oriRemoveListener = target[ZONE_SYMBOL_REMOVE_EVENT_LISTENER];
        }
        if (listener) {
          oriRemoveListener.call(target, READY_STATE_CHANGE, listener);
        }
        const newListener = target[XHR_LISTENER] = () => {
          if (target.readyState === target.DONE) {
            if (!data.aborted && target[XHR_SCHEDULED] && task.state === SCHEDULED) {
              const loadTasks = target[Zone2.__symbol__("loadfalse")];
              if (target.status !== 0 && loadTasks && loadTasks.length > 0) {
                const oriInvoke = task.invoke;
                task.invoke = function() {
                  const loadTasks2 = target[Zone2.__symbol__("loadfalse")];
                  for (let i2 = 0; i2 < loadTasks2.length; i2++) {
                    if (loadTasks2[i2] === task) {
                      loadTasks2.splice(i2, 1);
                    }
                  }
                  if (!data.aborted && task.state === SCHEDULED) {
                    oriInvoke.call(task);
                  }
                };
                loadTasks.push(task);
              } else {
                task.invoke();
              }
            } else if (!data.aborted && target[XHR_SCHEDULED] === false) {
              target[XHR_ERROR_BEFORE_SCHEDULED] = true;
            }
          }
        };
        oriAddListener.call(target, READY_STATE_CHANGE, newListener);
        const storedTask = target[XHR_TASK];
        if (!storedTask) {
          target[XHR_TASK] = task;
        }
        sendNative.apply(target, data.args);
        target[XHR_SCHEDULED] = true;
        return task;
      }
      function placeholderCallback() {
      }
      function clearTask(task) {
        const data = task.data;
        data.aborted = true;
        return abortNative.apply(data.target, data.args);
      }
      const openNative = patchMethod(XMLHttpRequestPrototype, "open", () => function(self2, args) {
        self2[XHR_SYNC] = args[2] == false;
        self2[XHR_URL] = args[1];
        return openNative.apply(self2, args);
      });
      const XMLHTTPREQUEST_SOURCE = "XMLHttpRequest.send";
      const fetchTaskAborting = zoneSymbol("fetchTaskAborting");
      const fetchTaskScheduling = zoneSymbol("fetchTaskScheduling");
      const sendNative = patchMethod(XMLHttpRequestPrototype, "send", () => function(self2, args) {
        if (Zone2.current[fetchTaskScheduling] === true) {
          return sendNative.apply(self2, args);
        }
        if (self2[XHR_SYNC]) {
          return sendNative.apply(self2, args);
        } else {
          const options = { target: self2, url: self2[XHR_URL], isPeriodic: false, args, aborted: false };
          const task = scheduleMacroTaskWithCurrentZone(XMLHTTPREQUEST_SOURCE, placeholderCallback, options, scheduleTask, clearTask);
          if (self2 && self2[XHR_ERROR_BEFORE_SCHEDULED] === true && !options.aborted && task.state === SCHEDULED) {
            task.invoke();
          }
        }
      });
      const abortNative = patchMethod(XMLHttpRequestPrototype, "abort", () => function(self2, args) {
        const task = findPendingTask(self2);
        if (task && typeof task.type == "string") {
          if (task.cancelFn == null || task.data && task.data.aborted) {
            return;
          }
          task.zone.cancelTask(task);
        } else if (Zone2.current[fetchTaskAborting] === true) {
          return abortNative.apply(self2, args);
        }
      });
    }
  });
  Zone.__load_patch("geolocation", (global2) => {
    if (global2["navigator"] && global2["navigator"].geolocation) {
      patchPrototype(global2["navigator"].geolocation, ["getCurrentPosition", "watchPosition"]);
    }
  });
  Zone.__load_patch("PromiseRejectionEvent", (global2, Zone2) => {
    function findPromiseRejectionHandler(evtName) {
      return function(e2) {
        const eventTasks = findEventTasks(global2, evtName);
        eventTasks.forEach((eventTask) => {
          const PromiseRejectionEvent = global2["PromiseRejectionEvent"];
          if (PromiseRejectionEvent) {
            const evt = new PromiseRejectionEvent(evtName, { promise: e2.promise, reason: e2.rejection });
            eventTask.invoke(evt);
          }
        });
      };
    }
    if (global2["PromiseRejectionEvent"]) {
      Zone2[zoneSymbol("unhandledPromiseRejectionHandler")] = findPromiseRejectionHandler("unhandledrejection");
      Zone2[zoneSymbol("rejectionHandledHandler")] = findPromiseRejectionHandler("rejectionhandled");
    }
  });

  // js/app.js
  var providerWithZone = new WebTracerProvider({
    resource: new Resource({
      [SemanticResourceAttributes.SERVICE_NAME]: "roll-dice-ui"
    })
  });
  providerWithZone.addSpanProcessor(new SimpleSpanProcessor(new ConsoleSpanExporter()));
  providerWithZone.addSpanProcessor(new SimpleSpanProcessor(new OTLPTraceExporter({
    url: "http://localhost:4318/v1/traces"
  })));
  providerWithZone.register({
    contextManager: new ZoneContextManager()
  });
  registerInstrumentations({
    instrumentations: [
      new DocumentLoadInstrumentation(),
      new XMLHttpRequestInstrumentation({
        ignoreUrls: [/localhost/],
        propagateTraceHeaderCorsUrls: [
          "http://localhost:3000"
        ]
      })
    ],
    tracerProvider: providerWithZone
  });
})();
/*! Bundled license information:

zone.js/fesm2015/zone.js:
  (**
   * @license Angular v12.0.0-next.0
   * (c) 2010-2020 Google LLC. https://angular.io/
   * License: MIT
   *)
  (**
   * @license
   * Copyright Google LLC All Rights Reserved.
   *
   * Use of this source code is governed by an MIT-style license that can be
   * found in the LICENSE file at https://angular.io/license
   *)
*/

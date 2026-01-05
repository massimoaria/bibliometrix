biblioShot <- function(
  url,
  file = "screenshot_hq.png",
  zoom = 2,
  vwidth = 1600,
  vheight = 1000
) {
  # 1. Normalizzazione URL
  if (!grepl("^[a-zA-Z]+://", url)) {
    url <- paste0(
      "file://",
      normalizePath(url, mustWork = TRUE, winslash = "/")
    )
  }

  # 2. Avvio sessione
  b <- chromote::default_chromote_object()
  s <- b$new_session(width = vwidth, height = vheight)
  on.exit(s$close())

  s$Page$navigate(url)
  s$Page$loadEventFired()

  # 3. Attesa iniziale per caricamento script
  Sys.sleep(1)

  # 4. JavaScript per forzare il rendering (usando paste0 per R)
  js_cmd <- paste0(
    '
    (function() {
      var el = document.querySelector(".html-widget") || document.body;
      el.style.width = "',
    vwidth,
    'px";
      el.style.height = "',
    vheight,
    'px";
      
      if (window.network) {
          window.network.redraw(); 
          window.network.fit();
      }
      
      // Forza il rendering dei pixel leggendo il canvas
      var canvas = document.querySelector("canvas");
      return canvas ? canvas.toDataURL() : "no-canvas";
    })()
  '
  )

  # Esecuzione sincrona del comando JS
  s$Runtime$evaluate(js_cmd)

  # Piccola attesa post-rendering
  Sys.sleep(0.5)

  # 5. Screenshot con scala forzata
  s$screenshot(
    filename = file,
    scale = zoom,
    show = FALSE,
    wait_ = TRUE
  )

  message(paste("Screenshot salvato:", normalizePath(file)))
  return(invisible(normalizePath(file)))
}

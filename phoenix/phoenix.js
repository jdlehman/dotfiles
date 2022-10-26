const resizeAmount = 50;

// ================
// Resize bindings
// ================
Key.on("l", ["cmd", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.width += resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("h", ["cmd", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.width -= resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("j", ["cmd", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.height += resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("k", ["cmd", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.height -= resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("l", ["ctrl", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.width -= resizeAmount;
    frame.x += resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("h", ["ctrl", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.width += resizeAmount;
    frame.x -= resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("j", ["ctrl", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.height -= resizeAmount;
    frame.y += resizeAmount;
    window.setFrame(frame);
  }
});

Key.on("k", ["ctrl", "option"], () => {
  const window = Window.focused();

  if (window) {
    const frame = window.frame();
    frame.height += resizeAmount;
    frame.y -= resizeAmount;
    window.setFrame(frame);
  }
});

// ================
// Quadrant bindings
// ================
Key.on("h", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: 0,
      width: screen.width / 2,
      height: screen.height
    });
  }
});

Key.on("l", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: screen.width / 2,
      y: 0,
      width: screen.width / 2,
      height: screen.height
    });
  }
});

Key.on("k", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: 0,
      width: screen.width,
      height: screen.height / 2
    });
  }
});

Key.on("j", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: screen.height / 2,
      width: screen.width,
      height: screen.height / 2
    });
  }
});

Key.on("y", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: 0,
      width: screen.width / 2,
      height: screen.height / 2
    });
  }
});

Key.on("u", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: screen.width / 2,
      y: 0,
      width: screen.width / 2,
      height: screen.height / 2
    });
  }
});

Key.on("b", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: screen.height / 2,
      width: screen.width / 2,
      height: screen.height / 2
    });
  }
});

Key.on("n", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: screen.width / 2,
      y: screen.height / 2,
      width: screen.width / 2,
      height: screen.height / 2
    });
  }
});

Key.on("space", ["ctrl", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setFrame({
      x: 0,
      y: 0,
      width: screen.width,
      height: screen.height
    });
  }
});

// ================
// Placement bindings
// ================
Key.on("space", ["ctrl", "option", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setTopLeft({
      x: screen.x + screen.width / 2 - window.frame().width / 2,
      y: screen.y + screen.height / 2 - window.frame().height / 2
    });
  }
});

Key.on("h", ["ctrl", "option", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setTopLeft({
      x: 0,
      y: screen.y + screen.height / 2 - window.frame().height / 2
    });
  }
});

Key.on("l", ["ctrl", "option", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setTopLeft({
      x: screen.width - window.frame().width,
      y: screen.y + screen.height / 2 - window.frame().height / 2
    });
  }
});

Key.on("j", ["ctrl", "option", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setTopLeft({
      x: screen.x + screen.width / 2 - window.frame().width / 2,
      y: screen.height - window.frame().height
    });
  }
});

Key.on("k", ["ctrl", "option", "cmd"], () => {
  const screen = Screen.main().flippedVisibleFrame();
  const window = Window.focused();

  if (window) {
    window.setTopLeft({
      x: screen.x + screen.width / 2 - window.frame().width / 2,
      y: 0
    });
  }
});

window.RevealSlideContainer = function () {
  return {
    id: "RevealSlideContainer",
    init: function (deck) {
      const slides = deck.getSlides();
      slides.forEach(function(slide) {
        if (!slide.classList.contains("slide")) {
          return;
        }
        const slideContainer = document.createElement("div");
        slideContainer.classList.add("slide-container");
        [...slide.children].forEach(function(child) {
          slideContainer.appendChild(child);
        });
        slide.appendChild(slideContainer);
      })
    },
  };
};


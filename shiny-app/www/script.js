$(function() {
  console.log("script.js is here!");

  // Initialize draggable blocks with smooth transitions
  $('.draggable-block').draggable({
    revert: 'invalid',
    start: function() {
      $(this).css('transform', 'scale(1.1)');
    },
    stop: function() {
      $(this).css('transform', 'scale(1)');
    }
  });

  console.log("Finished initializing draggable blocks...");

  // Initialize droppable areas
  $('.drag-area').droppable({
    accept: '.draggable-block',
    hoverClass: 'hovered',
    drop: function(event, ui) {
      var blockId = ui.draggable.attr('id');
      var areaId = $(this).attr('id');

      var blocksDrawn = Shiny.shinyapp.$inputValues.blocksDrawn || {
        area1: [],
        area2: [],
        area3: [],
        area4: []
      };

      for (var area in blocksDrawn) {
        blocksDrawn[area] = blocksDrawn[area].filter(function(id) { return id !== blockId; });
      }

      if (areaId === 'area-1') blocksDrawn.area1.push(blockId);
      if (areaId === 'area-2') blocksDrawn.area2.push(blockId);
      if (areaId === 'area-3') blocksDrawn.area3.push(blockId);
      if (areaId === 'area-4') blocksDrawn.area4.push(blockId);

      Shiny.setInputValue('blocksDrawn', blocksDrawn, {priority: 'event'});
    }
  });
});

Shiny.addCustomMessageHandler('reinitializeDraggables', function(message) {
    console.log("Custom message received to reinitialize draggable elements.");

    if ($('.draggable-block').length > 0) {
        $('.draggable-block').draggable({
            revert: 'invalid',
            start: function() {
                $(this).css('transform', 'scale(1.1)');
            },
            stop: function() {
                $(this).css('transform', 'scale(1)');
            }
        });

        console.log("Draggable blocks reinitialized via custom message.");
    } else {
        console.log("No draggable blocks found to initialize.");
    }
});

document.addEventListener("DOMContentLoaded", function () {
    console.log("script.js loaded successfully!");

    function drawArrow(startElement, endElement, arrowElement, startAnchor, endAnchor, startShiftX = 0, startShiftY = 0, endShiftX = 0, endShiftY = 0, lineStyle = 'solid', dualArrowheads = false) {
        if (!startElement || !endElement || !arrowElement) {
            console.error("One of the elements is missing:", { startElement, endElement, arrowElement });
            return;
        }

        const svgContainer = document.getElementById("main-container");
        const svgRect = svgContainer.getBoundingClientRect();
        const startRect = startElement.getBoundingClientRect();
        const endRect = endElement.getBoundingClientRect();

        console.log("Start Rect:", startRect);
        console.log("End Rect:", endRect);

        let x1, y1, x2, y2;

        const labelHeight = 15; // Updated label height

        switch (startAnchor) {
            case 'bottom':
                x1 = startRect.left + startRect.width / 2 - svgRect.left + startShiftX;
                y1 = startRect.bottom - svgRect.top + startShiftY;
                break;
            case 'right':
                x1 = startRect.right - svgRect.left + startShiftX;
                y1 = startRect.top + startRect.height / 2 - svgRect.top + startShiftY;
                break;
            case 'left':
                x1 = startRect.left - svgRect.left + startShiftX;
                y1 = startRect.top + startRect.height / 2 - svgRect.top + startShiftY;
                break;
            case 'top':
                x1 = startRect.left + startRect.width / 2 - svgRect.left + startShiftX;
                y1 = startRect.top - svgRect.top + startShiftY;
                break;
            default: // 'center'
                x1 = startRect.left + startRect.width / 2 - svgRect.left + startShiftX;
                y1 = startRect.top + startRect.height / 2 - svgRect.top + startShiftY;
        }

        switch (endAnchor) {
            case 'top':
                x2 = endRect.left + endRect.width / 2 - svgRect.left + endShiftX;
                y2 = endRect.top - svgRect.top + endShiftY;
                break;
            case 'right':
                x2 = endRect.right - svgRect.left + endShiftX;
                y2 = endRect.top + endRect.height / 2 - svgRect.top + endShiftY;
                break;
            case 'left':
                x2 = endRect.left - svgRect.left + endShiftX;
                y2 = endRect.top + endRect.height / 2 - svgRect.top + endShiftY;
                break;
            case 'bottom':
                x2 = endRect.left + endRect.width / 2 - svgRect.left + endShiftX;
                y2 = endRect.bottom - svgRect.top + endShiftY;
                break;
            default: // 'center'
                x2 = endRect.left + endRect.width / 2 - svgRect.left + endShiftX;
                y2 = endRect.top + endRect.height / 2 - svgRect.top + endShiftY;
        }

        // Adjust y1 and y2 for label overlap avoidance
        if (startElement.id === 'area-3') {
            if (startAnchor === 'top') {
                y1 = y1 - labelHeight;
            }
        }

        if (endElement.id === 'area-3') {
            if (endAnchor === 'top') {
                y2 = y2 - labelHeight;
            }
        }

        arrowElement.setAttribute("x1", x1);
        arrowElement.setAttribute("y1", y1);
        arrowElement.setAttribute("x2", x2);
        arrowElement.setAttribute("y2", y2);
        arrowElement.setAttribute("marker-end", "url(#arrowhead)");

        if (lineStyle === 'dashed') {
            arrowElement.setAttribute("stroke-dasharray", "5,5");
        } else {
            arrowElement.removeAttribute("stroke-dasharray");
        }

        if (dualArrowheads) {
            arrowElement.setAttribute("marker-start", "url(#arrowhead-start)");
        } else {
            arrowElement.removeAttribute("marker-start");
        }

        console.log("Arrow drawn:", { x1, y1, x2, y2 });
    }

    function updateArrows() {
        console.log("Updating arrows");
        drawArrow(
            document.getElementById("area-1"),
            document.getElementById("area-2"),
            document.getElementById("arrow-z-x"),
            'bottom', 'right', -20, 0, 0, -20, 'dashed', true // Shift left and up, dashed, dual arrowheads
        );
        drawArrow(
            document.getElementById("area-1"),
            document.getElementById("area-3"),
            document.getElementById("arrow-z-w"),
            'bottom', 'top'
        );
        drawArrow(
            document.getElementById("area-1"),
            document.getElementById("area-4"),
            document.getElementById("arrow-z-y"),
            'bottom', 'left', 20, 0, 0, -20 // Shift right and up
        );
        drawArrow(
            document.getElementById("area-2"),
            document.getElementById("area-3"),
            document.getElementById("arrow-x-w"),
            'right', 'top', 0, 20, -20, 0 // Shift up
        );
        drawArrow(
            document.getElementById("area-2"),
            document.getElementById("area-4"),
            document.getElementById("arrow-x-y"),
            'right', 'left', 0, 0, 0, 0 // Shift up and right
        );
        drawArrow(
            document.getElementById("area-3"),
            document.getElementById("area-4"),
            document.getElementById("arrow-w-y"),
            'top', 'left', 20, 0, 0, 20
        );
    }

    window.addEventListener("resize", updateArrows);
    updateArrows();
});

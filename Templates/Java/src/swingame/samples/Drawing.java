package swingame.samples;

import  java.util.List;
import  java.util.ArrayList;
import  java.awt.Point;
import  java.awt.Color;


/**
 * A Drawing is a collection of Shapes that are drawn and modified
 * by a DrawingController.
 *
 * @author   Andrew Cain
 */
 public class Drawing
 {
     private List<Shape> _shapes = new ArrayList<Shape>();
     private Shape _selectedShape;

     public Shape getSelectedShape()
     {
         return _selectedShape;
     }

     public void addShape(Shape toAdd)
     {
         _shapes.add(toAdd);
     }

     /**
     * Sets the selected Shape to the Point specified.
     *
     * @param point The location of the Shape to select
     */
     public void selectShape(Point point)
     {
         _selectedShape = null;
         for (Shape s: _shapes)
         {
             if (s.isAt(point))
             {
                 _selectedShape = s;
                 return;
             }
         }
     }

     /**
     * Draws all of the Shapes within the Drawing.
     */
     void draw()
     {
         for (Shape s : _shapes)
         {
             s.draw();
         }
     }
 }


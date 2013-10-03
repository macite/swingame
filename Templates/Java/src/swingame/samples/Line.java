package swingame.samples;

import java.awt.Point;
import swingame.Graphics;

/**
 * The Line is drawn from the Shape's position
 * to the alternate corner based on width and 
 * height of the shape.
 *
 * @author  Andrew Cain
 */
 class Line extends Shape
 {
     public void draw()
     {
         Point pos = getPosition();
         
         float x = (float)pos.getX();
         float y = (float)pos.getY();
         float x2 = (float)(pos.getX() + getWidth());
         float y2 = (float)(pos.getY() + getHeight());

         Graphics.drawLine(getColor(), x, y, x2, y2);
     }
 }

package swingame.samples;

import java.awt.Color;
import swingame.Graphics;
import swingame.Input;
import swingame.KeyCode;

/**
 * A Rectangle Shape.
 *
 * @author  Andrew
 */
 class Rectangle extends Shape
 {
     public void draw()
     {
         Graphics.fillRectangle(getColor(), (float)getPosition().getX(), (float)getPosition().getY(), getWidth(), getHeight());
     }
 }
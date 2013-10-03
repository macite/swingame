package swingame.samples;

import java.awt.Color;
import java.awt.Point;

/**
 * A Shape is an abstract class used to define the basic features that all
 * Shapes in the ShapeDrawer program.
 *
 * @author   Andrew Cain
 */
 public abstract class Shape
 {
     private Color _color;

     public Color getColor()
     {
         return _color;
     }
     
     public void setColor(Color value)
     {
        _color = value;
     }
     
     private Point _position;

     public Point getPosition()
     {
         return _position;
     }
     
     public void setPosition(Point value)
     {
         _position = value;
     }
     
     private int _width;

     public int getWidth()
     {
         return _width;
     }
     
     public void setWidth(int value)
     {
         _width = value;
     }
     
     private int _height;

     public int getHeight()
     {
         return _height;
     }
     
     public void setHeight(int value)
     {
         _height = value;
     }

     public boolean isAt(Point point)
     {
         return point.getX() >= getPosition().getX() && 
             point.getX() < getPosition().getX() + getWidth() && 
             point.getY() >= getPosition().getY() && 
             point.getY() < getPosition().getY() + getHeight();
     }

     public abstract void draw();
 }

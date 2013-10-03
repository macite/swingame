package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Graphics;
import swingame.Input;
import swingame.KeyCode;
import swingame.MouseButton;

/**
 * The DrawingController is used be the user interface to control the actions performed on
 * a Drawing object. This is responsible for handling the user input and turning this into
 * actions is performs on the Drawing object.
 *
 * @author  Andrew Cain
 */
 public class DrawingController
 {
     private enum DrawingElements
     {
         LINE, RECTANGLE, ELLIPSE;
     }

     private final int _defaultWidth = 10;
     private final int _defaultHeight = 10;
     private static final Color _defaultColor = Color.RED;

     private DrawingElements _adding = DrawingElements.RECTANGLE;
     private Drawing _controlling;

     public Drawing getControlling()
     {
         return _controlling;
     }
     
     public void setControlling(Drawing value)
     {
         _controlling = value;
     }

     public DrawingController()
     {
         this(new Drawing());
     }

     public DrawingController(Drawing drawing)
     {
         _controlling = drawing;
     }

     public void selectShape(Point point)
     {
         _controlling.selectShape(point);
     }

     public void changeColor()
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             float hue, sat, bri;
             hue = Graphics.hueOf(s.getColor());
             sat = Graphics.saturationOf(s.getColor());
             bri = Graphics.brightnessOf(s.getColor());

             hue += 0.01f;
             if (hue > 1) hue = hue - 1;

             s.setColor(Graphics.hSBColor(hue, sat, bri));
         }
     }

     public void changeSize(int dw, int dh)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             s.setWidth(s.getWidth() + dw);
             s.setHeight(s.getHeight() + dh);
         }
     }

     public void moveTo(Point point)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             s.setPosition(point);
         }            
     }

     public void move(int dx, int dy)
     {
         Shape s = _controlling.getSelectedShape();
         if (s != null)
         {
             Point p = s.getPosition();
             p.setLocation(p.getX() + dx, p.getY() + dy);
         }
     }

     public void addNewShape(Point point)
     {
         Shape s;
         switch (_adding)
         {
             case RECTANGLE: s = new Rectangle(); break;
             case LINE: s = new Line(); break;
             default: return;
         }

         Point p = new Point();
         p.setLocation(point.getX() - _defaultWidth / 2, point.getY() - _defaultHeight / 2);

         s.setPosition(p);
         s.setColor(_defaultColor);
         s.setWidth(_defaultWidth);
         s.setHeight(_defaultHeight);

         _controlling.addShape(s);
     }

     public void draw()
     {
         _controlling.draw();
     }

     public void handleInput()
     {
         if (Input.mouseDown(MouseButton.LEFT_BUTTON))
         {
             if (Input.keyDown(KeyCode.VK_SHIFT))
             {
                 moveTo((Point)Input.mousePosition().clone());
             }
             else
             {
                 addNewShape(Input.mousePosition());
             }
         }
         
         if (Input.mouseClicked(MouseButton.RIGHT_BUTTON))
         {
             selectShape(Input.mousePosition());
         }

         if (Input.keyTyped(KeyCode.VK_R)) _adding = DrawingElements.RECTANGLE;
         if (Input.keyTyped(KeyCode.VK_L)) _adding = DrawingElements.LINE;
         if (Input.keyTyped(KeyCode.VK_E)) _adding = DrawingElements.ELLIPSE;

         if (Input.keyDown(KeyCode.VK_SHIFT))
         {
             if (Input.keyDown(KeyCode.VK_LEFT)) changeSize(-1, 0);
             if (Input.keyDown(KeyCode.VK_RIGHT)) changeSize(1, 0);
             if (Input.keyDown(KeyCode.VK_UP)) changeSize(0, -1);
             if (Input.keyDown(KeyCode.VK_DOWN)) changeSize(0, 1);
         }
         else
         {
             if (Input.keyDown(KeyCode.VK_LEFT)) move(-1, 0);
             if (Input.keyDown(KeyCode.VK_RIGHT)) move(1, 0);
             if (Input.keyDown(KeyCode.VK_UP)) move(0, -1);
             if (Input.keyDown(KeyCode.VK_DOWN)) move(0, 1);
         }

         if (Input.keyDown(KeyCode.VK_C)) changeColor();
     }
 }


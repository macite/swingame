package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Graphics;
import swingame.Input;
import swingame.MouseButton;

/**
 * The Mandelbrot class provides a viewer of the mandelbrot set that
 * make use of the SwinGame API for drawing. This class provides a 
 * main method which coordinates the generation and drawing of the
 * Mandelbrot set.
 *
 * 29-08-2008: Version 1 completed.
 *
 * @author Andrew Cain
 */
public class Mandelbrot 
{
    /**
    * The maximum number of iterations in the calculation of the colors to
    * draw for the mandelbrot set.
    *
    * Altering this value will alter the number of colors shown in the set,
    * and the time needed to draw the set.
    */
    public static final int MAX_ITERATIONS = 1000;
    
    /**
    * The width of the screen. Altering this will impact on the size
    * of the set and the time needed to draw it to the screen.
    *
    * SCREEN_WIDTH and SCREEN_HEIGHT should have a 4:3 ratio.
    */
    public static final int SCREEN_WIDTH = 320;

    /**
    * The height of the screen. Altering this will impact on the size
    * of the set and the time needed to draw it to the screen.
    *
    * SCREEN_WIDTH and SCREEN_HEIGHT should have a 4:3 ratio.
    */
    public static final int SCREEN_HEIGHT = 240;
    
    /**
    * Calculates the color of the given x0,y0 coordinate. Both the 
    * x and y are coordinates from within the mandelbrot set.
    *
    * @param x0     The x coordinate of the location in the set
    * @param y0     The y coordinate of the location in the set
    * @return       The Color to draw for this location in the set.
    */
    public static Color mandelbrotColor(float x0, float y0)
    {
        float x, y, xtemp;
        int iteration;

        x = x0;  //the x co-ordinate of pixel in Mandelbrot set coordinates
        y = y0;  //the y co-ordinate of pixel in Mandelbrot set coordinates

        //Calculate the number of iterations for this location in the set
        iteration = 0;
        while ((x*x + y*y <= 2*2)  &&  (iteration < MAX_ITERATIONS))
        {
            xtemp = x*x - y*y + x0;
            y = 2*x*y + y0;

            x = xtemp;

            iteration = iteration + 1;
        }
 
        return mapColor(iteration);
    }
    
    /**
    * Maps a number of iterations to a SwinGame Color. 
    *
    * @param iteration  The interation count to be mapped to a color.
    * @return           The color used to represent that number of iterations.
    */
    public static Color mapColor(int iteration)
    {
        if (iteration == MAX_ITERATIONS)
            return Color.BLACK; //if we stopped looking... make it black
        else
        {
            //use interation / max iteration to determine an appropriate hue
            // adjust into the blue range for 0 iterations.
            float hue = (iteration / (float)MAX_ITERATIONS) + (180 / 360.0f);
  
            if (hue > 1) hue = hue - 1; //correct for values beond 1.
  
            return Graphics.hSBColor(hue, 1.0f, 0.9f);
        }
    }
    
    /**
    * Draw the mandelbrot set to the screen, taking the coordinates of the set as
    * parameters. The set is drawn to the entire screen.
    *
    * @param setX       The x coordinate of the top left corner of the set
    * @param setY       The y coordinate of the top left corner of the set
    * @param setWidth   The width of the set to display on screen
    * @param setHeight  The height of the set to display on screen
    */
    public static void drawMandelbrot(float setX, float setY, float setWidth, float setHeight)
    {
        Color color;
        float scaleWidth, scaleHeight;
        
        scaleWidth = setWidth / SCREEN_WIDTH;
        scaleHeight = setHeight / SCREEN_HEIGHT;
        
        for (int x = 0; x < SCREEN_WIDTH; x++)
        {
            for (int y = 0; y < SCREEN_HEIGHT; y++)
            {   
                color = mandelbrotColor(setX + x * scaleWidth, setY + y * scaleHeight);
                Graphics.drawPixel(color, x, y);
            }
        }
    }
    
    /**
    * Draws the mandelbrot to the screen and handles user input, allowing the user to 
    * zoom in and out using the mouse.
    *
    * @param args       The command line arguments - not used.
    */
    public static void main(String[] args)
    {
        //setup the coordinates of the set to be viewed on screen
        float setX, setY, setWidth, setHeight;
        
        setX = -2.5f;
        setY = -1.5f;
        setWidth = 4.0f;
        setHeight = 3.0f;

        //Open a new Graphics Window - using the constants
        Graphics.openGraphicsWindow("Mandelbrot", SCREEN_WIDTH, SCREEN_HEIGHT);

        //draw to buffer
        drawMandelbrot(setX, setY, setWidth, setHeight);

        //Game loop
        do
        {
            Input.processEvents();

            if(Input.mouseClicked(MouseButton.LEFT_BUTTON))
            {
                //Zoom in...
                Point mouse;
		        mouse = Input.mousePosition();

                // location of click - offset to center
		        setX = setX + (float)mouse.getX() / SCREEN_WIDTH * setWidth - setWidth / 4.0f;
                setY = setY + (float)mouse.getY() / SCREEN_HEIGHT * setHeight - setHeight / 4.0f;
                
                // Zoom by viewing 1/4 of old set
                setWidth = setWidth / 2.0f;
		        setHeight = setHeight / 2.0f;

                //redraw to buffer
		        drawMandelbrot(setX, setY, setWidth, setHeight);
            }

            //Draw buffer to screen
            Graphics.refreshScreen(60);
        } while (false == Input.windowCloseRequested());

        //Close the window
        Graphics.close();
    }
}

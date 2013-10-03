package swingame.samples;

import java.awt.Color;
import swingame.Graphics;
import swingame.Input;
import swingame.KeyCode;

/**
 * The IntegerSortAnimator objects store a collection
 * of integer values that can be sorted and randomized. 
 * 
 * @author acain
 */
public class IntegerSortAnimator 
{
    private final int _max;
    private int[] _data;
    
    /**
    * Creates a new IntegerSortAnimator with an specified number of 
    * elements from 0 to a max value.
    *
    * @param count  The number of values to store in this animator
    * @param max    The largest value that can be stored in the data for this animator
    */
    public IntegerSortAnimator(int count, int max)
    {
	    _max = max;
        _data = new int[count];
	    randomize();
    }
    
    /**
    * Provides access to the array contained within the IntegerSortAnimator.
    *
    * @return The array containing the data within the object - direct access
    */
    public int[] getData()
    {
        return _data;
    }

    /**
    * Randomize the data within the IntegerSortAnimator. This generates a new random set of data
    * for the animator to sort. The new values will be between 0 and max.
    */
    public void randomize() 
    {
        for(int i = 0; i < _data.length; i++)
        {
            _data[i] = (int)(Math.random() * _max);
        }
    }
    
    /**
    * Draws the data into the current SwinGame graphics window. This 
    * drawing takes the entire screen.
    */
    public void showData()
    {
        Graphics.clearScreen();

        int w = Graphics.screenWidth() / _data.length;
        float scaleHeight = Graphics.screenHeight() / (float)_max;

        for(int i = 0; i < _data.length; i++)
        {         
                float x = w * i;
                float y = (_max - _data[i]) * scaleHeight;
                int h = (int)Math.ceil(_data[i] * scaleHeight);

                Graphics.fillRectangle(Color.RED, x, y, w, h);
        }

        Graphics.refreshScreen(60);
    }
    
    /**
    * Sorts the data within the IntegerSortAnimator, updating the display 
    * at each step of the algorithm.
    */
    public void sort()
    {
        sort(true);
    }
    
    /**
    * Sorts the data within the IntegerSortAnimator. The show parameter
    * is used to indicate if this should be drawn to the screen while it
    * progresses.
    *
    * @param show   Indicates if the animator should redraw the data as it 
    * sorts.
    */
    public void sort(boolean show)    
    {
        for(int i = _data.length - 1; i >= 0; i--)
        {
            for(int j = 0; j < i; j++)
            {
                if(show)
                {
                    Input.processEvents();
                    if(Input.windowCloseRequested()) return;
                    if(Input.keyTyped(KeyCode.VK_ESCAPE)) return;
                }
                
                if(_data[j] > _data[j+1])
                {
                    int temp = _data[j];
                    _data[j] = _data[j+1];
                    _data[j+1] = temp;
                    
                    if(show) showData();
                }
            }
        }
    }
}

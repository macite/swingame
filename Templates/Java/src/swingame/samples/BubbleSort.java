package swingame.samples;

import swingame.KeyCode;
import swingame.Input;

public class BubbleSort implements SortAlgorithm
{
    public void sort(IntegerSortAnimator2 data, boolean show)
    {
        int[] theData = data.getData();
        
        for(int i = theData.length - 1; i >= 0; i--)
        {
            for(int j = 0; j < i; j++)
            {
                if(show)
                {
                    Input.processEvents();
                    if(Input.windowCloseRequested()) return;
                    if(Input.keyTyped(KeyCode.VK_ESCAPE)) return;
                }
                
                if(theData[j] > theData[j+1])
                {
                    int temp = theData[j];
                    theData[j] = theData[j+1];
                    theData[j+1] = temp;
                    
                    if(show) data.showData();
                }
            }
        }
    }
}
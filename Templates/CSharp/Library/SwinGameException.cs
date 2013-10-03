//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SwinGameException
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// Change History:
//
// Version 2.0:
// - 2009-01-20: Andrew: Added version histroy 
//                       to newly created classes
//
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// A SwinGameException will occur if something has gone wrong in 
    /// SwinGame. In many cases this will indicate that you are using the API
    /// in a way that was not intended. If you believe there is a bug, or need
    /// help with an Exception please use the forum at http://www.swingame.com
    /// </summary>
    public class SwinGameException : Exception
    {
        /// <summary>
        /// SwinGameExceptions
        /// </summary>
        /// <param name="message">Exception Message</param>
        public SwinGameException(string message)
            : base(message)
        { }
    }
}

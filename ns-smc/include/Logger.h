#ifndef TwinPeaks2018_Logger_h
#define TwinPeaks2018_Logger_h

namespace TwinPeaks2018
{

// Verbosity levels
enum class Verbosity
{
    low, medium, high
};


/* Use singleton pattern */
class Logger
{
    public:

        // Global instance
        static Logger logger;

    private:

        // Verbosity setting
        Verbosity verbosity;

    public:

        // Constructor: set the verbosity level
        Logger(const Verbosity& _verbosity);

        // Change the verbosity level
        void set_verbosity(Verbosity&& _verbosity);

        // TODO: Actually use the logger

};


} // namespace TwinPeaks2018

#endif


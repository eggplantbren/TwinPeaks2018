#include "Logger.h"
#include <utility>

namespace TwinPeaks2018
{

Logger Logger::logger(Verbosity::medium);

Logger::Logger(const Verbosity& _verbosity)
:verbosity(_verbosity)
{

}

void Logger::set_verbosity(Verbosity&& _verbosity)
{
    std::swap(verbosity, _verbosity);
}


} // namespace TwinPeaks2018


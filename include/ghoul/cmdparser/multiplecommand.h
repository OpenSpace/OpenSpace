/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __GHOUL___MULTIPLECOMMAND___H__
#define __GHOUL___MULTIPLECOMMAND___H__

#include <ghoul/cmdparser/commandlinecommand.h>

namespace ghoul::cmdparser {

template <typename... T>
class MultipleCommand : public CommandlineCommand {};

/**
 * This class represents a command that can called multiple times in a given commandline
 * and has a single argument of respective type \p T. Each time the command is called, the
 * converted value is appended to a vector that has been passed in the constructor. The
 * template class \p T must be convertable using an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 *
 * \see MultipleCommandZeroArguments
 */
template <class T>
class MultipleCommand<T> : public CommandlineCommand {
public:
    /**
     * This constructor uses all four parameters. These can be of the same or different
     * types. The command does not take ownership of the vectors.
     *
     * \param ptr1 The reference to the parameters that will be set when this command is
     *        executed
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     * \param parameterList The explanation of the parameters that this command expects.
     *        Is presented to the user upon request by the CommandlineParser
     */
    MultipleCommand(std::vector<T>& ptr1, std::string name, std::string shortName = "",
        std::string infoText = "", std::string parameterList = "");

    /**
     * Executes this MultipleCommand and stores the values passed as \p parameters into
     * the pointers passed through the constructor.
     *
     * \param parameters The parameters for this MultipleCommand
     *
     * \throw CommandExecutionException If one parameter has the wrong type that was not
     *        detected in the checkParameters method
     */
    void execute(const std::vector<std::string>& parameters) override;

    /**
     * Checks whether all of the \p parameters have the correct types.
     *
     * \param parameters The list of parameters that are to be checked
     *
     * \throw CommandParameterException If any of the parameters have the wrong type
     */
    void checkParameters(const std::vector<std::string>& parameters) const override;

protected:
    std::vector<T>& _ptr1;
};

/**
 * This class represents a command that can called multiple times in a given commandline
 * and has 2 arguments of respective types \p T and \p U. Each time the command is called,
 * the converted value is appended to a vector that has been passed in the constructor.
 * The template classes \p T, \p U, \p V, and \p U must be convertable using an
 * `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 *
 * \see MultipleCommandZeroArguments
 */
template <class T, class U>
class MultipleCommand<T, U> : public CommandlineCommand {
public:
    /**
     * This constructor uses all four parameters. These can be of the same or different
     * types. The command does not take ownership of the vectors.
     *
     * \param ptr1 The reference to the parameters that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameters that will be set when this
     *        command is executed
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     * \param parameterList The explanation of the parameters that this command expects.
     *        Is presented to the user upon request by the CommandlineParser
     */
    MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2, std::string name,
        std::string shortName = "", std::string infoText = "",
        std::string parameterList = "");

    /**
     * Executes this MultipleCommand and stores the values passed as \p parameters into
     * the pointers passed through the constructor.
     *
     * \param parameters The parameters for this MultipleCommand
     *
     * \throw CommandExecutionException If one parameter has the wrong type that was not
     *        detected in the checkParameters method
     */
    void execute(const std::vector<std::string>& parameters) override;

    /**
     * Checks whether all of the \p parameters have the correct types.
     *
     * \param parameters The list of parameters that are to be checked
     *
     * \throw CommandParameterException If any of the parameters have the wrong type
     */
    void checkParameters(const std::vector<std::string>& parameters) const override;

protected:
    std::vector<T>& _ptr1;
    std::vector<U>& _ptr2;
};

/**
 * This class represents a command that can called multiple times in a given commandline
 * and has 3 arguments of respective types \p T, \p U, and \p V. Each time the command is
 * called, the converted value is appended to a vector that has been passed in the
 * constructor. The template classes \p T, \p U, \p V, and \p U must be convertable using
 * an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 * \tparam V The typename of the third argument type
 *
 * \see MultipleCommandZeroArguments
 */
template <class T, class U, class V>
class MultipleCommand<T, U, V> : public CommandlineCommand {
public:
    /**
     * This constructor uses all four parameters. These can be of the same or different
     * types. The command does not take ownership of the vectors.
     *
     * \param ptr1 The reference to the parameters that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameters that will be set when this
     *        command is executed
     * \param ptr3 The reference to the third parameters that will be set when this
     *        command is executed
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     * \param parameterList The explanation of the parameters that this command expects.
     *        Is presented to the user upon request by the CommandlineParser
     */
    MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2, std::vector<V>& ptr3,
        std::string name, std::string shortName = "", std::string infoText = "",
        std::string parameterList = "");

    /**
     * Executes this MultipleCommand and stores the values passed as \p parameters into
     * the pointers passed through the constructor.
     *
     * \param parameters The parameters for this MultipleCommand
     *
     * \throw CommandExecutionException If one parameter has the wrong type that was not
     *        detected in the checkParameters method
     */
    void execute(const std::vector<std::string>& parameters) override;

    /**
     * Checks whether all of the \p parameters have the correct types.
     *
     * \param parameters The list of parameters that are to be checked
     *
     * \throw CommandParameterException If any of the parameters have the wrong type
     */
    void checkParameters(const std::vector<std::string>& parameters) const override;

protected:
    std::vector<T>& _ptr1;
    std::vector<U>& _ptr2;
    std::vector<V>& _ptr3;
};

/**
 * This class represents a command that can called multiple times in a given commandline
 * and has 4 arguments of respective types \p T, \p U, \p V, and \p U. Each time the
 * command is called, the converted value is appended to a vector that has been passed in
 * the constructor. The template classes \p T, \p U, \p V, and \p U must be convertable
 * using an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 * \tparam V The typename of the third argument type
 * \tparam W The typename of the fourth argument type
 *
 * \see MultipleCommandZeroArguments
 */
template <class T, class U, class V, class W>
class MultipleCommand<T, U, V, W> : public CommandlineCommand {
public:
    /**
     * This constructor uses all four parameters. These can be of the same or different
     * types. The command does not take ownership of the vectors.
     *
     * \param ptr1 The reference to the parameters that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameters that will be set when this
     *        command is executed
     * \param ptr3 The reference to the third parameters that will be set when this
     *        command is executed
     * \param ptr4 The reference to the fourth parameters that will be set when this
     *        command is executed
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     * \param parameterList The explanation of the parameters that this command expects.
     *        Is presented to the user upon request by the CommandlineParser
     */
    MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2, std::vector<V>& ptr3,
        std::vector<W>& ptr4, std::string name, std::string shortName = "",
        std::string infoText = "", std::string parameterList = "");

    /**
     * Executes this MultipleCommand and stores the values passed as \p parameters into
     * the pointers passed through the constructor.
     *
     * \param parameters The parameters for this MultipleCommand
     *
     * \throw CommandExecutionException If one parameter has the wrong type that was not
     *        detected in the checkParameters method
     */
    void execute(const std::vector<std::string>& parameters) override;

    /**
     * Checks whether all of the \p parameters have the correct types.
     *
     * \param parameters The list of parameters that are to be checked
     *
     * \throw CommandParameterException If any of the parameters have the wrong type
     */
    void checkParameters(const std::vector<std::string>& parameters) const override;

protected:
    std::vector<T>& _ptr1;
    std::vector<U>& _ptr2;
    std::vector<V>& _ptr3;
    std::vector<W>& _ptr4;
};

/**
 * This class represents a command with zero arguments which can be called multiple times
 * in a given commandline. The `int` pointer will contain the number of how often the
 * command was present in the command-line.
 *
 * \see MultipleCommand
 */
class MultipleCommandZeroArguments : public CommandlineCommand {
public:
    /**
     * This constructor requests one `int` parameter that returns the number of
     * times this command was executed.
     *
     * \param nExecutions The reference to the int that will be set to the number of
     *        executions
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     */
    MultipleCommandZeroArguments(int& nExecutions, std::string name,
        std::string shortName = "", std::string infoText = "");

    /**
     * Increases the `int` value passed in the constructor by one per execution.
     */
    void execute(const std::vector<std::string>& parameters) override;

protected:
    int& _ptr;
};

} // namespace ghoul::cmdparser

#include "multiplecommand.inl"

#endif // __GHOUL___MULTIPLECOMMAND___H__

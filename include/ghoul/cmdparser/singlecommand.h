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

#ifndef __GHOUL___SINGLECOMMAND___H__
#define __GHOUL___SINGLECOMMAND___H__

#include <ghoul/cmdparser/commandlinecommand.h>

#include <optional>

namespace ghoul::cmdparser {

template <typename... T>
class SingleCommand : public CommandlineCommand {};

/**
 * This class represents a command that can occur only once in a given commandline and has
 * a single argument of type \p T, which must be convertable using a `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 *
 * \see SingleCommandZeroArguments
 */
template <typename T>
class SingleCommand<T> : public CommandlineCommand {
public:
    /**
     * This constructor uses a single parameter.
     *
     * \param ptr1 The reference to the parameter that will be set when this command is
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
    SingleCommand(std::optional<T>& ptr1, std::string name, std::string shortName = "",
        std::string infoText = "", std::string parameterList = "");

    /**
     * Executes this SingleCommand and stores the values passed as \p parameters into the
     * pointers passed through the constructor.
     *
     * \param parameters The parameters for this SingleCommand
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
    std::optional<T>& _ptr1;
};

/**
 * This class represents a command that can occur only once in a given commandline and has
 * 2 aguments of respective types \p T and \p U. The template classes \p T and \p U must
 * be convertable using an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 *
 * \see SingleCommandZeroArguments
 */
template <typename T, typename U>
class SingleCommand<T, U> : public CommandlineCommand {
public:
    /**
     * This constructor uses two parameters.
     *
     * \param ptr1 The reference to the parameter that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameter that will be set when this
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
    SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2, std::string name,
        std::string shortName = "", std::string infoText = "",
        std::string parameterList = "");

    /**
     * Executes this SingleCommand and stores the values passed as \p parameters into the
     * pointers passed through the constructor.
     *
     * \param parameters The parameters for this SingleCommand
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
    std::optional<T>& _ptr1;
    std::optional<U>& _ptr2;
};

/**
 * This class represents a command that can occur only once in a given commandline and has
 * 3 arguments of respective types \p T, \p U, and \p V. The template classes \p T, \p U,
 * and \p V must be convertable using an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 * \tparam V The typename of the third argument type
 *
 * \see SingleCommandZeroArguments
 */
template <typename T, typename U, typename V>
class SingleCommand<T, U, V> : public CommandlineCommand {
public:
    /**
     * This constructor uses three parameters.
     *
     * \param ptr1 The reference to the parameter that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameter that will be set when this
     *        command is executed
     * \param ptr3 The reference to the third parameter that will be set when this command
     *        is executed
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     * \param parameterList The explanation of the parameters that this command expects.
     *        Is presented to the user upon request by the CommandlineParser
     */
    SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2, std::optional<V>& ptr3,
        std::string name, std::string shortName = "", std::string infoText = "",
        std::string parameterList = "");

    /**
     * Executes this SingleCommand and stores the values passed as \p parameters into the
     * pointers passed through the constructor.
     *
     * \param parameters The parameters for this SingleCommand
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
    std::optional<T>& _ptr1;
    std::optional<U>& _ptr2;
    std::optional<V>& _ptr3;
};

/**
 * This class represents a command that can occur only once in a given commandline and has
 * 4 arguments of respective types \p T, \p U, \p V, and \p U. The command tries to
 * convert the parameters to the appropriate types and stores them. The template classes
 * \p T, \p U, \p V, and \p U must be convertable using an `std::stringstream`.
 *
 * \tparam T The typename of the first argument type
 * \tparam U The typename of the second argument type
 * \tparam V The typename of the third argument type
 * \tparam W The typename of the fourth argument type
 *
 * \see SingleCommandZeroArguments
 */
template <typename T, typename U, typename V, typename W>
class SingleCommand<T, U, V, W> : public CommandlineCommand {
public:
    /**
     * This constructor takes four parameters.
     *
     * \param ptr1 The reference to the parameter that will be set when this command is
     *        executed
     * \param ptr2 The reference to the second parameter that will be set when this
     *        command is executed
     * \param ptr3 The reference to the third parameter that will be set when this command
     *        is executed
     * \param ptr4 The reference to the fourth parameter that will be set when this
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
    SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2, std::optional<V>& ptr3,
        std::optional<W>& ptr4, std::string name, std::string shortName = "",
        std::string infoText = "", std::string parameterList = "");

    /**
     * Executes this SingleCommand and stores the values passed as \p parameters into the
     * pointers passed through the constructor.
     *
     * \param parameters The parameters for this SingleCommand
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
    std::optional<T>& _ptr1 = nullptr;
    std::optional<U>& _ptr2 = nullptr;
    std::optional<V>& _ptr3 = nullptr;
    std::optional<W>& _ptr4 = nullptr;
};


/**
 * This class represents a command with zero arguments that can only occur once in a given
 * commandline. The bool pointer will be set to `true`, if the command is executed.
 */
class SingleCommandZeroArguments : public CommandlineCommand {
public:
    /**
     * This constructor uses one `bool` parameter that is set to `true` if this command
     * has been found. The command does not take ownership of the passed value.
     *
     * \param ptr The reference to the bool that will be set to `true` on execution
     * \param name The full name for this command. Has to start with a `-` in order to be
     *        valid
     * \param shortName The (optional) short name for this command. If it is provided, it
     *        has to start with a `-` in order to be valid
     * \param infoText The info text that will be presented to the user if it is requested
     *        by the CommandlineParser
     */
    SingleCommandZeroArguments(std::optional<bool>& ptr, std::string name,
        std::string shortName = "", std::string infoText = "");

    /**
     * Sets the `bool` value passed in the constructor to `true`.
     */
    void execute(const std::vector<std::string>& /*parameters*/) override;

protected:
    std::optional<bool>& _ptr;
};

} // namespace ghoul::cmdparser

#include "singlecommand.inl"

#endif // __GHOUL___SINGLECOMMAND___H__

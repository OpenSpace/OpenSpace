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

#ifndef __GHOUL___COMMANDLINECOMMAND___H__
#define __GHOUL___COMMANDLINECOMMAND___H__

#include <ghoul/misc/boolean.h>
#include <ghoul/misc/exception.h>
#include <string>
#include <vector>

namespace ghoul::cmdparser {

/**
 * A command is an operation that can be called via command line arguments on program
 * startup. As this class is virtual, it has to be derived from with the #execute method
 * implemented. The CommandlineCommand are used by adding them to the CommandlineParser,
 * using the method CommandlineParser::addCommand. The common way of using the commands is
 * to pass a variable of appropriate type to the CommandlineCommand by reference, which
 * gets set to the correct value when the command is executed. Within the parser, the
 * command's `name` and `shortName` must be unique.
 *
 * For use, see the templated classes SingleCommand and MultipleCommand which are capable
 * of setting basic types that are convertible from string using a `std::stringstream`.
 */
class CommandlineCommand {
public:
    BooleanType(MultipleCalls);

    /**
     * Exception that gets thrown if an error occurs in the CommandlineCommand::execute
     * that could not be checked in the CommandlineCommand::checkParameters method.
     */
    struct CommandExecutionException final : public RuntimeError {
        explicit CommandExecutionException(std::string msg);
    };

    /**
     * Exception that gets thrown if an error occurs in the
     * ComandlineCommand::checkParameters.
     */
    struct CommandParameterException final : public RuntimeError {
        explicit CommandParameterException(std::string msg);
    };

    /**
     * The constructor which saves the arguments to own member variables.
     *
     * \param name The (long) name of the parameter. For example `--command1`
     * \param shortName The abbreviated name of the parameter. For example `-c`
     * \param infoText A short text (preferably one line) explaining what the command
     *        does. Used in the CommandlineParser::displayHelp method
     * \param parameterList A user-readable description which parameters are used and
     *        supported. This is used in the CommandlineParser::displayUsage and
     *        CommandlineParser::displayHelp methods
     * \param argumentNum The number of arguments this command accepts
     * \param allowMultipleCalls If this argument is `true` it signals the
     *        CommandlineParser that it should allow multiple instances of this
     *        CommandlineCommand in a single command line
     *
     * \pre \p name must not be empty
     * \pre \p name must start with a '-'
     * \pre If the \p shortName is not empty, it must start with a '-'
     */
    CommandlineCommand(std::string name, std::string shortName = "",
        std::string infoText = "", std::string parameterList = "", int argumentNum = 1,
        MultipleCalls allowMultipleCalls = MultipleCalls::No);

    virtual ~CommandlineCommand() = default;

    /**
     * Return the full name of this command.
     *
     * \return The full name of this command
     */
    const std::string& name() const;

    /**
     * Returns the short name of this command.
     *
     * \return The short name of this command
     */
    const std::string& shortName() const;

    /**
     *
     * Returns the parameter list necessary for the #usage method.
     *
     * \return The parameter list this command expects
     */
    const std::string& parameterList() const;

    /**
     * Returns a short description used in the CommandlineParser::displayHelp and
     * CommandlineParser::displayUsage methods.
     *
     * \return A short description of this command
     */
    const std::string& infoText() const;

    /**
     * Returns the number of accepted arguments for this command.
     *
     * \return The number of accepted arguments for this command
     */
    int argumentNumber() const;

    /**
     * Returns if the command can be called more than once in a single command line.
     *
     * \return If the command can be called more than once in a single command line
     */
    MultipleCalls allowsMultipleCalls() const;

    /**
     * Executes this command with the given parameters. Each subclass must implement this
     * abstract method and perform all actions within it.
     *
     * \param parameters The parameters needed for the execution of this command. By the
     *        time this method is called, the parameters have already been verified by the
     *        #checkParameters method
     *
     * \throw CommandExecutionException If one parameter has the wrong type that was not
     *        detected in the checkParameters method
     */
    virtual void execute(const std::vector<std::string>& parameters) = 0;

    /**
     * Checks the param for consistency and correct amount. The basic implementation only
     * checks for the correct number of param. If you want to test for other conditions
     * (for example type), overwrite this method in the derived class.
     *
     * \param param The param which should be tested
     *
     * \throw CommandParameterException If any of the parameters have the wrong type
     */
    virtual void checkParameters(const std::vector<std::string>& param) const;

    /**
     * Returns the usage part for the help of this CommandlineCommand. Used in the
     * CommandlineParser::usage method.
     *
     * \return The usage part for the help of this command
     */
    virtual std::string usage() const;

    /**
     * Returns the help-part for a command. Used in the CommandlineParser::help method.
     *
     * \return The help-part for a command
     */
    virtual std::string help() const;

protected:
    /**
     * Casts the string value \p s into the type `T`. If the conversion fails, an
     * CommandException is thrown. The conversion is done via an `std::stringstream` so it
     * can only cast those types supported by the stream.
     *
     * \tparam T The type of the value which should be converted
     * \param s The `std::string` representation of the value
     *
     * \throw CommandException If the conversion failed
     * \pre \p s must not be empty
     */
    template <class T>
    T cast(const std::string& s) const;

    /**
     * Checks if the string value \p s can be cast into the type `T`. It only returns
     * `true` for those values that can be converted using a `std::stringstream`.
     *
     * \tparam T The type of the value which should be converted
     * \param s The `std::string` representation of the value
     * \return `true` if the value can be converted, `false` otherwise
     */
    template <class T>
    [[nodiscard]] bool is(const std::string& s) const;

    /// Name of the command used on command-line level
    const std::string _name;
    /// The short name of this command which is also usable (usually an abbreviation)
    const std::string _shortName;
    /// A description of the command; used in the #help method
    const std::string _infoText;
    /// The parameter list necessary for the #usage method
    const std::string _parameterList;
    /// Name used as a prefix for logging
    const std::string _loggerCat;
    /// Stores the number of arguments this command accepts
    int _nArguments;
    /// Stores, if the command can be called multiple times in a single command line
    const MultipleCalls _allowsMultipleCalls;
};

} // namespace ghoul::cmdparser

#include "commandlinecommand.inl"

#endif // __GHOUL___COMMANDLINECOMMAND___H__

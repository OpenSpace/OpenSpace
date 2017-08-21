import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../../utils/helpers';
import styles from './NumericInput.scss';
import Input from '../Input/Input';

class NumericInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      value: props.value,
      showTextInput: false,
    };

    this.onChange = this.onChange.bind(this);
    this.toggleInput = this.toggleInput.bind(this);
  }

  componentWillReceiveProps({ value }) {
    this.setState({ value });
  }

  /**
   * callback for input
   * @param event InputEvent
   */
  onChange(event) {
    const { value } = event.currentTarget;
    const { max, min } = this.props;

    if (value > max || value < min) return;

    // update state so that input is re-rendered with new content
    this.setState({ value });

    // send to the onChange (if any)!
    this.props.onChange(event);
  }

  get showTextInput() {
    return this.props.inputOnly || this.state.showTextInput;
  }

  toggleInput() {
    if (this.props.disableInput) return;

    this.setState({ showTextInput: !this.state.showTextInput });
  }

  render() {
    const { value } = this.state;

    if (this.showTextInput) {
      return (
        <Input
          {...excludeKeys(this.props, 'disableInput inputOnly')}
          type="number"
          value={value}
          onChange={this.onChange}
          onBlur={this.toggleInput}
          autoFocus
        />
      );
    }

    const { placeholder, className, wide, id, min, max } = this.props;
    const doNotInclude = 'wide onChange value className type disableInput inputOnly';
    const inheritedProps = excludeKeys(this.props, doNotInclude);

    return (
      <div
        className={`${styles.inputGroup} ${wide ? styles.wide : ''}`}
        onDoubleClick={this.toggleInput}
      >
        <input
          {...inheritedProps}
          type="range"
          value={value}
          className={`${className} ${styles.range}`}
          onChange={this.onChange}
          style={{ '--min': min, '--max': max, '--value': value }}
        />
        <label htmlFor={id} className={`${styles.rangeLabel}`}>
          {placeholder}
        </label>
        <span className={styles.value} onClick={this.toggleInput} role="button" tabIndex={0}>
          {value}
        </span>
      </div>
    );
  }
}

NumericInput.idCounter = 0;

NumericInput.propTypes = {
  className: PropTypes.string,
  disableInput: PropTypes.bool,
  id: PropTypes.string.isRequired,
  inputOnly: PropTypes.bool,
  max: PropTypes.number,
  min: PropTypes.number,
  onChange: PropTypes.func,
  placeholder: PropTypes.string.isRequired,
  step: PropTypes.number,
  value: PropTypes.number,
  wide: PropTypes.bool,
};

NumericInput.defaultProps = {
  className: '',
  disableInput: false,
  id: `numericinput-${NumericInput.idCounter++}`,
  inputOnly: false,
  max: 100,
  min: 0,
  onChange: () => {},
  step: 1,
  value: 0,
  wide: true,
};

export default NumericInput;

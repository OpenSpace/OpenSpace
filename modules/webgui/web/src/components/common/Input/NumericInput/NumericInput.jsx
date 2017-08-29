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
      id: `numericinput-${Input.nextId}`,
      hoverHint: null,
    };

    this.onChange = this.onChange.bind(this);
    this.onHover = this.onHover.bind(this);
    this.onLeave = this.onLeave.bind(this);
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

    // update state so that input is re-rendered with new content - optimistic ui change
    this.setState({ value });

    // send to the onChange (if any)!
    this.props.onChange(event);
  }

  onHover(event) {
    if (this.props.noHoverHint || this.props.disabled) {
      return;
    }

    // get bounds for input to calc hover percentage
    const { left, right } = event.currentTarget.getBoundingClientRect();
    const { clientX } = event;
    const hoverHint = (clientX - left) / (right - left);
    this.setState({ hoverHint });
  }

  onLeave() {
    this.setState({ hoverHint: null });
  }

  get showTextInput() {
    return this.props.inputOnly || this.state.showTextInput;
  }

  toggleInput() {
    if (this.props.disableInput) return;

    this.setState({ showTextInput: !this.state.showTextInput });
  }

  render() {
    const { value, id, hoverHint } = this.state;

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

    const { placeholder, className, label, wide, min, max } = this.props;
    const doNotInclude = 'wide onChange value className type disableInput inputOnly';
    const inheritedProps = excludeKeys(this.props, doNotInclude);

    return (
      <div
        className={`${styles.inputGroup} ${wide ? styles.wide : ''}`}
        onDoubleClick={this.toggleInput}
        onMouseMove={this.onHover}
        onMouseLeave={this.onLeave}
      >
        { hoverHint !== null && (
          <div className={styles.hoverHint} style={{ width: `${100 * hoverHint}%` }} />
        )}
        <input
          {...inheritedProps}
          id={id}
          type="range"
          value={value}
          className={`${className} ${styles.range}`}
          style={{ '--min': min, '--max': max, '--value': value }}
          onChange={this.onChange}
        />
        <label htmlFor={id} className={`${styles.rangeLabel}`}>
          { label || placeholder }
        </label>
        <span className={styles.value} onClick={this.toggleInput} role="button" tabIndex={0}>
          {value}
        </span>
      </div>
    );
  }
}

NumericInput.propTypes = {
  className: PropTypes.string,
  disabled: PropTypes.bool,
  disableInput: PropTypes.bool,
  inputOnly: PropTypes.bool,
  label: PropTypes.node,
  max: PropTypes.number,
  min: PropTypes.number,
  noHoverHint: PropTypes.bool,
  onChange: PropTypes.func,
  placeholder: PropTypes.string.isRequired,
  step: PropTypes.number,
  value: PropTypes.oneOfType([PropTypes.number, PropTypes.string]),
  wide: PropTypes.bool,
};

NumericInput.defaultProps = {
  className: '',
  disabled: false,
  disableInput: false,
  inputOnly: false,
  label: null,
  max: 100,
  min: 0,
  noHoverHint: false,
  onChange: () => {},
  step: 1,
  value: 0,
  wide: true,
};

export default NumericInput;

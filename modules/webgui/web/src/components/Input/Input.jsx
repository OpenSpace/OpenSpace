import React, { Component } from 'react';
import PropTypes from 'prop-types';

import styles from './Input.scss';

class Input extends Component {
  static get nextId() {
    // eslint-disable-next-line no-plusplus
    return Input.idCounter++;
  }

  constructor(props) {
    super(props);

    this.state = {
      value: props.value,
      id: `input-${Input.nextId}`,
    };

    this.onChange = this.onChange.bind(this);
  }

  /**
   * callback for input
   * @param event InputEvent
   */
  onChange(event) {
    const { value } = event.target;

    // update state so that input is re-rendered with new content
    this.setState(prev => Object.assign(prev, { value }));

    // send to the onChange (if any)!
    this.props.onChange(event);
  }

  get hasInput() {
    return this.state.value !== '';
  }

  render() {
    const { type, placeholder, className, wide, disabled, autocomplete } = this.props;
    const { value, id } = this.state;
    return (
      <div className={styles.group}>
        <input
          autoComplete={autocomplete}
          className={`${className} ${styles.input}
                      ${this.hasInput && styles.hasinput}
                      ${wide && styles.wide}`}
          disabled={disabled}
          id={id}
          placeholder={placeholder}
          onChange={this.onChange}
          value={value}
          type={type}
        />
        <label htmlFor={id} className={`${styles.label} ${this.hasInput && styles.hasinput}`}>
          {placeholder}
        </label>
      </div>
    );
  }
}

Input.idCounter = 0;

Input.propTypes = {
  autocomplete: PropTypes.bool,
  onChange: PropTypes.func,
  className: PropTypes.string,
  disabled: PropTypes.bool,
  placeholder: PropTypes.string.isRequired,
  type: PropTypes.string,
  value: PropTypes.string,
  wide: PropTypes.bool,
};

Input.defaultProps = {
  autocomplete: false,
  onChange: () => {},
  className: '',
  disabled: false,
  type: 'text',
  value: '',
  wide: false,
};

export default Input;


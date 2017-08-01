import React, { Component } from 'react';
import PropTypes from 'prop-types';

import Icon from '../Icon/Icon';

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
      id: props.id,
    };

    this.onChange = this.onChange.bind(this);
    this.clear = this.clear.bind(this);
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

  clear() {
    this.setState({ value: '' });
  }

  get hasInput() {
    return this.state.value !== '';
  }

  /**
   * filter out props that shouldn't be inherited by the input element
   * @returns {*}
   */
  get inheritProps() {
    const doNotInclude = 'wide onChange loading value clearable'.split(' ');
    return Object.keys(this.props)
      // actually filter out the keywords
      .filter(key => !doNotInclude.includes(key))
      // create new props object to return
      .reduce((newProps, key) => {
        const prop = {};
        prop[key] = this.props[key];
        return Object.assign(newProps, prop);
      }, {});
  }

  render() {
    const { placeholder, className, wide, loading, clearable } = this.props;
    const { value, id } = this.state;
    return (
      <div className={styles.group}>
        <input
          {...this.inheritProps}
          className={`${className} ${styles.input}
                      ${this.hasInput && styles.hasinput}
                      ${loading && styles.loading}
                      ${wide && styles.wide}`}
          id={id}
          onChange={this.onChange}
          value={value}
        />
        <label htmlFor={id} className={`${styles.label} ${this.hasInput && styles.hasinput}`}>
          {placeholder}
        </label>
        { clearable && (
          <Icon
            icon="cancel"
            className={`small ${styles.clearbutton} ${this.hasInput && styles.hasinput}`}
            onClick={this.clear}
            tabindex="0"
            role="button"
            title="Clear input field"
          />
        )}
      </div>
    );
  }
}

Input.idCounter = 0;

Input.propTypes = {
  onChange: PropTypes.func,
  className: PropTypes.string,
  clearable: PropTypes.bool,
  id: PropTypes.string,
  loading: PropTypes.bool,
  placeholder: PropTypes.string.isRequired,
  value: PropTypes.string,
  wide: PropTypes.bool,
};

Input.defaultProps = {
  onChange: () => {},
  className: '',
  clearable: false,
  id: `input-${Input.nextId}`,
  loading: false,
  value: '',
  wide: true,
};

export default Input;


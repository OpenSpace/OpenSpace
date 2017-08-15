import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../utils/helpers';
import styles from './NumericInput.scss';

class NumericInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      value: props.value,
    };

    this.onChange = this.onChange.bind(this);
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

    // update state so that input is re-rendered with new content
    this.setState({ value });

    // send to the onChange (if any)!
    this.props.onChange(event);
  }

  render() {
    const doNotInclude = 'wide onChange value className type';
    const inheritedProps = excludeKeys(this.props, doNotInclude);
    const { placeholder, className, wide, id, min, max } = this.props;
    const { value } = this.state;
    return (
      <div className={`${styles.inputGroup} ${wide ? styles.wide : ''}`}>
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
        <span className={styles.value}>{value}</span>
      </div>
    );
  }
}

NumericInput.idCounter = 0;

NumericInput.propTypes = {
  className: PropTypes.string,
  id: PropTypes.string.isRequired,
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
  id: `numericinput-${NumericInput.idCounter++}`,
  max: 100,
  min: 0,
  onChange: () => {},
  step: 1,
  value: 0,
  wide: true,
};

export default NumericInput;

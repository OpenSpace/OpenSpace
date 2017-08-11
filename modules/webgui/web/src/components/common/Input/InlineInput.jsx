import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Input from './Input';
import styles from './InlineInput.scss';

class InlineInput extends Component {
  constructor(props) {
    super(props);
    this.state = { value: props.value };

    this.onChange = this.onChange.bind(this);
    this.setWidthElement = this.setWidthElement.bind(this);
  }

  componentWillReceiveProps({ value }) {
    this.setState({ value });
  }

  onChange(event) {
    const { value } = event.currentTarget;
    this.setState({ value });
    this.props.onChange(event);
  }

  setWidthElement(elem) {
    this.widthElement = elem;
  }

  get width() {
    return this.widthElement && this.widthElement.offsetWidth;
  }

  render() {
    const { value } = this.state;
    return (
      <div className={styles.wrapper}>
        <span className={styles.hidden} ref={this.setWidthElement}>{ value }</span>
        <input
          {...this.props}
          className={`${styles.input} ${this.props.className}`}
          value={value}
          onChange={this.onChange}
          style={{ width: `${this.width}px` }}
        />
      </div>
    );
  }
}

InlineInput.propTypes = {
  className: PropTypes.string,
  id: PropTypes.string,
  onChange: PropTypes.func,
  type: PropTypes.string,
  value: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
};

InlineInput.defaultProps = {
  className: '',
  id: `input-${Input.nextId}`,
  onChange: () => {},
  type: 'text',
  value: '',
};

export default InlineInput;

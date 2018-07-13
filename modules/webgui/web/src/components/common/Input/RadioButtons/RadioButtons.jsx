import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../../utils/helpers';
import styles from './RadioButtons.scss';
import Label from '../../Label/Label';

// TODO: intitial double render
class RadioButtons extends Component {
  constructor(props) {
    super(props);

    this.state = {
        selectedOption: ''
    };

    this.onChange = this.onChange.bind(this);
  }

  onChange(event) {    
    const selectedOption = event.target.value;
    this.setState({ selectedOption });

    this.props.onChange(selectedOption);
  }

  componentDidUpdate() {
    if (this.state.selectedOption === '' && this.props.defaultOption) {
        this.setState({ selectedOption: this.props.defaultOption });
        this.props.onChange(this.props.defaultOption);
    }
  }

  render() {
    const inheritProps = excludeKeys(this.props, 'onChange label options defaultOption');
    const { options, label, isHorizontal } = this.props;

    return (
        <div className={styles.wrapper}>
            <Label size={'medium'}>
              { label }
            </Label>
            <div className={`${isHorizontal ? styles.optionsHorizontal : styles.optionsVertical}`}>
            {options.map(option => (
                <label key={option}>
                    <input type="radio" 
                        value={option} 
                        checked={this.state.selectedOption === option} 
                        onChange={this.onChange} />
                    {option}
                </label>
            ))}
            </div>
        </div>
    );
  }
}

RadioButtons.propTypes = {
  defaultOption: PropTypes.string,
  isHorizontal: PropTypes.bool,
  label: PropTypes.node,
  onChange: PropTypes.func,
  options: PropTypes.arrayOf(PropTypes.string).isRequired,
};

RadioButtons.defaultProps = {
  isHorizontal: true,
  label: 'Select',
  onChange: () => {},
};

export default RadioButtons;

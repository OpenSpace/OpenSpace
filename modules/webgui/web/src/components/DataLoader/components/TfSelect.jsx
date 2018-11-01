import React from 'react';
import PropTypes from 'prop-types';
import styles from './TfSelect.scss';
import Label from '../../common/Label/Label';
import FlexColumn from '../components/FlexColumn';
import Row from '../../common/Row/Row';
import Tf from './Tf';

class TfSelect extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      selectedTfPath: '',
    };

    this.onClick = this.onClick.bind(this);
    this.selectFirst = this.selectFirst.bind(this);
    this.formatByLabel = this.formatByLabel.bind(this);
  }

  componentDidUpdate() {
    const { defaultToFirst, presets } = this.props;

    if (defaultToFirst && presets.length > 0 && this.state.selectedPath === '') {
      this.selectFirst();
    }
  }

  onClick(path) {
    this.setState({ selectedTfPath: path });
    this.props.onSelect(path);
  }

  // handleClick(event) {
  //   const clickedSrc = event.target.src;
  //   this.setState({ selectedImgSrc: clickedSrc });
  //   this.props.onSelect(clickedSrc);
  // }

  selectFirst() {
    const firstPath = this.props.presets[0].path;
    this.setState({ selectedTfPath: firstPath }, this.props.onSelect(firstPath));
  }

  formatByLabel(presets) {
    const byLabel = [];

    for (const preset of presets) {
      let found = false;

      for (const el of byLabel) {
        if (preset.label === el.label) {
          el.presets.push(preset);
          found = true;
        }
      }

      if (found === false) {
        const element = {
          label: preset.label,
          presets: [preset],
        };

        byLabel.push(element);
      }
    }

    return byLabel;
  }

  render() {
    const { presets } = this.props;
    const { selectedTfPath } = this.state;
    const presetsByLabel = this.formatByLabel(presets);
    const hasPresets = presetsByLabel.length > 0;

    return (
      <div>
        {hasPresets && presetsByLabel.map(el => (
          <FlexColumn key={el.label} className={styles.container}>
            <Label size="small" className={styles.label}>{el.label}</Label>
            <div className={styles.row}>
              {el.presets.map((preset) => {
                let formattedMaxVal = preset.maxValue;
                if (Number.isInteger(formattedMaxVal)) {
                  formattedMaxVal = Number.toFixed(1);
                }

                return (
                  <Tf
                    key={preset.path}
                    img={preset.img}
                    path={preset.path}
                    onClick={this.onClick}
                    selected={selectedTfPath === preset.path}
                    min={preset.minValue}
                    max={formattedMaxVal}
                  />
                );
              },
              )}
            </div>
          </FlexColumn>
        ))}
      </div>
    );
  }
}

TfSelect.propTypes = {
  defaultToFirst: PropTypes.bool,
  presets: PropTypes.arrayOf(PropTypes.shape({
    img: PropTypes.string,
    path: PropTypes.string,
    label: PropTypes.string,
    minValue: PropTypes.string,
    maxValue: PropTypes.string,
  })),
  onSelect: PropTypes.func.isRequired,
};

TfSelect.defaultProps = {
  defaultToFirst: true,
  presets: [{}],
};

export default TfSelect;
